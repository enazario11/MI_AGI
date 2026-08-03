#libraries
library(tidyverse)
library(here)
library(terra)
library(sf)
library(rnaturalearth)
library(tidyterra)
library(doParallel)
library(foreach)
library(patchwork)
source(here("functions/oxy_demand_functions_test.R"))

#load tpref and oxythresh data
sp_dat_tpref <- readRDS(here("data/agi/sp_dat_tpref.rds"))
sp_dat_oxythresh <- readRDS(here("data/agi/sp_dat_oxythresh.rds"))

  #combine to one df
oxytemp <- sp_dat_oxythresh %>% select(c(Common.name, thresh_med, thresh_min, thresh_quant))
tpref_oxythresh <- merge(sp_dat_tpref, oxytemp, by = "Common.name", all = TRUE)
agi_temp <- tpref_oxythresh %>% select(c(Common.name, min_depth, med_depth, quant_depth, Tpref_min, Tpref_med, Tpref_quant, thresh_min, thresh_med, thresh_quant))

#load fishbase coef data
agi_coef <- read.csv(here("data/agi/agi_coef.csv"))

#combine fishbase coefs and tpref/oxythresh values
agi_dat <- agi_coef %>% left_join(agi_temp, by = "Common.name")

#exploratory analyses of tpref and oxythresh
agi_viz <- agi_dat[!duplicated(agi_dat$Common.name), ]
agi_tpref <- agi_viz %>% 
  pivot_longer(
    cols = c(Tpref_min, Tpref_med, Tpref_quant), 
    names_to = "Tpref", 
    values_to = "value"
  )

nep_temp <- agi_tpref %>%
  filter(region == "nep") %>%
  mutate(Tpref = fct_relevel(Tpref, "Tpref_min", "Tpref_med", "Tpref_quant")) %>%
  ggplot(aes(Tpref, value)) + 
    geom_point(aes(color = Tpref), size = 4) + 
    facet_wrap(~Common.name) +
    theme_bw()+
    scale_color_manual(values = c("#5F9ABA", "#0B69A6", "#01448A")) + 
    xlab("") + 
    ggtitle("nep")

nwa_temp <- agi_tpref %>%
  filter(region == "nwa") %>%
  mutate(Tpref = fct_relevel(Tpref, "Tpref_min", "Tpref_med", "Tpref_quant")) %>%
  ggplot(aes(Tpref, value)) + 
    geom_point(aes(color = Tpref), shape = 17, size = 4) + 
    facet_wrap(~Common.name) +
    theme_bw()+
    scale_color_manual(values = c("#5F9ABA", "#0B69A6","#01448A")) + 
    xlab("") + 
    ggtitle("nwa")

nep_temp / nwa_temp

agi_oxythresh <- agi_viz %>% 
  pivot_longer(
    cols = c(thresh_min, thresh_med, thresh_quant), 
    names_to = "oxythresh", 
    values_to = "value"
  )

nep_ox <- agi_oxythresh %>%
  filter(region == "nep") %>%
  mutate(oxythresh = fct_relevel(oxythresh, "thresh_min", "thresh_med", "thresh_quant")) %>%
  ggplot(aes(oxythresh, value)) + 
    geom_point(aes(color = oxythresh), size = 4) + 
    facet_wrap(~Common.name) +
    theme_bw()+
    scale_color_manual(values = c("#01448A", "#0B69A6", "#5F9ABA"))+
    xlab("")+
    ggtitle("nep")

nwa_ox <- agi_oxythresh %>%
  filter(region == "nwa") %>%
  mutate(oxythresh = fct_relevel(oxythresh, "thresh_min", "thresh_med", "thresh_quant")) %>%
  ggplot(aes(oxythresh, value)) + 
    geom_point(aes(color = oxythresh), shape = 17, size = 4) + 
    facet_wrap(~Common.name) +
    theme_bw()+
    scale_color_manual(values = c("#01448A", "#0B69A6", "#5F9ABA"))+
    xlab("")+
    ggtitle("nwa")

nep_ox/nwa_ox

#load species hulls
sp_hull <- list.files("data/fish_hull", full.names = TRUE)

#calculate map of AGI 
AGI <- function(sp_name, weight, sex, season, enviro){ #consider adding enviro dat paths as arguments
  
  #isolate to species of interest
  agi_dat2 <- agi_dat %>% filter(Common.name == sp_name)
  agi_1row <- agi_dat2[1,]

  if(enviro == "bottom"){
    #load enviro data
    bo2_atm <- rast(here(past0("data/enviro/", agi_1row$region ,"/do/atm/", agi_1row$region, "_bo2_atm.nc")))

    bto_folder <- list.files(here(past0("data/enviro/", agi_1row$region, "/temp/raw/")), full.names = TRUE, pattern = "tob")
    bto <- rast(bto_folder[1])

    #get species hull
    sp_hull_path <- sp_hull[grepl(sp_name, sp_hull)]
    sp_hull_file <- st_read(paste0(sp_hull_path, "/", agi_1row$Genus[1], " ", agi_1row$Species[1], ".shp"))

    #crop enviro files to species hull
    bo2_crop <- crop(bo2_atm, sp_hull_file, mask = TRUE)
    bto_crop <- crop(bto, sp_hull_file, mask = TRUE)

    #calculate agi -- place holder weight of 1 kg
    oxy_demand <- OxyDemand(Tpref = agi_1row$Tpref_med, PO2_thresh = agi_1row$thresh_med, T_C = bto_crop, 
                            W = weight, K = agi_1row$K, Linf = agi_1row$Linf, LwA = agi_1row$LwA, LwB = agi_1row$LwB)
    
    agi <- bo2_crop/oxy_demand

    #take median across time
    agi_med <- median(agi, na.rm = TRUE)

  } else if (enviro == "pelagic"){
    #load o2 and temp rast data per depth 
    min_temp <- rast(here(paste0("data/enviro/", agi_1row$region ,"/temp/processed/hull_crop/", sp_name, "/min_depth/min_depth_temp.nc")))
    min_o2_atm <- rast(here(paste0("data/enviro/", agi_1row$region ,"/do/atm/hull_crop/", sp_name, "/min_depth/min_depth_o2.nc")))
    med_temp <- rast(here(paste0("data/enviro/", agi_1row$region ,"/temp/processed/hull_crop/", sp_name, "/med_depth/med_depth_temp.nc")))
    med_o2_atm <- rast(here(paste0("data/enviro/", agi_1row$region ,"/do/atm/hull_crop/", sp_name, "/med_depth/med_depth_o2.nc")))
    quant_temp <- rast(here(paste0("data/enviro/", agi_1row$region ,"/temp/processed/hull_crop/", sp_name, "/quant_depth/quant_depth_temp.nc")))
    quant_o2_atm <- rast(here(paste0("data/enviro/", agi_1row$region ,"/do/atm/hull_crop/", sp_name, "/quant_depth/quant_depth_o2.nc")))


    #agi min depth
      #calculate agi -- place holder weight of 1 kg
      min_o2_demand <- OxyDemand(Tpref = agi_1row$Tpref_min, PO2_thresh = agi_1row$thresh_min, T_C = min_temp, 
                              W = weight, K = agi_1row$K, Linf = agi_1row$Linf, LwA = agi_1row$LwA, LwB = agi_1row$LwB)
      
      agi_min_depth <- min_o2_atm/min_o2_demand

      #take median across time
      agi_min <- median(agi_min_depth, na.rm = TRUE)
      names(agi_min) <- "agi_min_depth"

    #agi med depth
      #calculate agi -- place holder weight of 1 kg
      med_o2_demand <- OxyDemand(Tpref = agi_1row$Tpref_med, PO2_thresh = agi_1row$thresh_med, T_C = med_temp, 
                              W = weight, K = agi_1row$K, Linf = agi_1row$Linf, LwA = agi_1row$LwA, LwB = agi_1row$LwB)
      
      agi_med_depth <- med_o2_atm/med_o2_demand

      #take median across time
      agi_med <- median(agi_med_depth, na.rm = TRUE)
      names(agi_med) <- "agi_med_depth"

    #agi 75% quantile depth
      #calculate agi -- place holder weight of 1 kg
      quant_o2_demand <- OxyDemand(Tpref = agi_1row$Tpref_quant, PO2_thresh = agi_1row$thresh_quant, T_C = quant_temp, 
                              W = weight, K = agi_1row$K, Linf = agi_1row$Linf, LwA = agi_1row$LwA, LwB = agi_1row$LwB)
      
      agi_quant_depth <- quant_o2_atm/quant_o2_demand

      #take median across time
      agi_quant <- median(agi_quant_depth, na.rm = TRUE)
      names(agi_quant) <- "agi_quant_depth"

    #combine agi
    agi_all <- c(agi_min, agi_med, agi_quant)
  }
}

#plot median agi values across survey period -- not specifying agi values for sex, season, or region
  #load land mask
land <- ne_countries(scale = "large", returnclass = "sf")

  #agi raster bottom species
agi_bot <- AGI(sp_name = "Atlantic cod", weight = 8000, enviro = "bottom")

ggplot() +
  geom_spatraster(data = agi_bot) +
  geom_sf(data = land, fill = "grey85", colour = "grey30", linewidth = 0.2) +
  coord_sf(xlim = as.vector(ext(agi_test))[1:2] + c(-2, 2),
           ylim = as.vector(ext(agi_test))[3:4] + c(-2, 2),
           expand = FALSE) +
  scale_fill_viridis_c(na.value = "transparent", name = "AGI", limits = c(0.5,2.5)) +
  labs(x = NULL, y = NULL) +
  theme_bw()
  
  #agi raster pelagic species
agi_pel <- AGI(sp_name = "Atlantic herring", weight = 300, enviro = "pelagic")

ggplot() +
  geom_spatraster(data = agi_pel) +
  geom_sf(data = land, fill = "grey85", colour = "grey30", linewidth = 0.2) +
  coord_sf(xlim = as.vector(ext(agi_test))[1:2] + c(-2, 2),
           ylim = as.vector(ext(agi_test))[3:4] + c(-2, 2),
           expand = FALSE) +
  scale_fill_viridis_c(na.value = "transparent", name = "AGI", limits = c(0.5,2.5)) +
  labs(x = NULL, y = NULL) +
  theme_bw() + 
  labs(title = names(agi_pel))
