#libraries
library(tidyverse)
library(here)
library(terra)
library(sf)
library(rnaturalearth)
library(tidyterra)
library(patchwork)
source(here("functions/oxy_demand_functions_test.R"))

#convert DO to atm across cropped rasters
#NWA #####
nwa_temp <- rast(here("data/enviro/nwa/temp/processed/temp_nwa_crop.nc"))
nwa_do <- rast(here("data/enviro/nwa/do/processed/o2_nwa_crop.nc"))
nwa_sal <- rast(here("data/enviro/nwa/salinity/processed/sal_nwa_crop.nc"))

nwa_do_atm <- rast_do_to_atm(do = nwa_do, t = nwa_temp, s = nwa_sal)

#NEP ######
nep_temp <- rast(here("data/enviro/nep/temp/processed/temp_nep_crop.nc"))
nep_do <- rast(here("data/enviro/nep/do/processed/o2_nep_crop.nc"))
nep_sal <- rast(here("data/enviro/nep/salinity/processed/sal_nep_crop.nc"))

nep_do_atm <- rast_do_to_atm(do = nep_do, t = nep_temp, s = nep_sal)

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

  if(agi_dat2$region == "nwa" && enviro == "bottom"){
    #load enviro data
    nwa_bo2 <- rast(here("data/enviro/nwa/do/atm/nwa_bo2_atm.nc"))
    nwa_bto <- rast(here("data/enviro/nwa/temp/raw/tob.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

    #get species hull
    sp_hull_path <- sp_hull[grepl(sp_name, sp_hull)]
    sp_hull_file <- st_read(paste0(sp_hull_path, "/", agi_dat2$Genus[1], " ", agi_dat2$Species[1], ".shp"))

    #crop enviro files to species hull
    nwa_bo2_crop <- crop(nwa_bo2, sp_hull_file, mask = TRUE)
    nwa_bto_crop <- crop(nwa_bto, sp_hull_file, mask = TRUE)

    #calculate agi -- place holder weight of 1 kg
    oxy_demand <- OxyDemand(Tpref = agi_dat2$Tpref_med, PO2_thresh = agi_dat2$thresh_med, T_C = nwa_bto_crop, 
                            W = weight, K = agi_dat2$K, Linf = agi_dat2$Linf, LwA = agi_dat2$LwA, LwB = agi_dat2$LwB)
    
    agi <- nwa_bo2_crop/oxy_demand

    #take median across time
    agi_med <- median(agi, na.rm = TRUE)

  } else if (agi_dat$region == "nwa" && enviro == "pelagic"){
    #load enviro data
    nwa_o2_atm <- rast(here("data/enviro/nwa/do/processed/o2_nwa_crop.nc"))
    nwa_temp <- rast(here("data/enviro/nwa/temp/processed/temp_nwa_crop.nc"))

    #load species hull
    sp_hull_path <- sp_hull[grepl(sp_name, sp_hull)]
    sp_hull_file <- st_read(paste0(sp_hull_path, "/", agi_dat2$Genus[1], " ", agi_dat2$Species[1], ".shp"))

    # get select depth layers
    min_layer <- which.min(abs(depth(nwa_temp) - agi_dat2$min_depth[1]))
    min_seq <- seq(from = min_layer, to = nlyr(nwa_temp), by = length(unique(depth(nwa_temp))))

    med_layer <- which.min(abs(depth(nwa_temp) - agi_dat2$med_depth[1]))
    med_seq <- seq(from = med_layer, to = nlyr(nwa_temp), by = length(unique(depth(nwa_temp))))
  
    quant_layer <- which.min(abs(depth(nwa_temp) - agi_dat2$quant_depth[1]))
    quant_seq <- seq(from = quant_layer, to = nlyr(nwa_temp), by = length(unique(depth(nwa_temp))))
  
    min_temp_rast <- nwa_temp[[min_seq]]
    min_o2_rast <- nwa_o2_atm[[min_seq]]
    med_temp_rast <- nwa_temp[[med_seq]]
    med_o2_rast <- nwa_o2_atm[[med_seq]]
    quant_temp_rast <- nwa_temp[[quant_seq]]
    quant_o2_rast <- nwa_o2_atm[[quant_seq]]

    #agi min depth
    min_temp_crop <- crop(min_temp_rast, hull, mask = TRUE)
    min_o2_crop <- crop(min_o2_rast, hull, mask = TRUE)

        #calculate agi -- place holder weight of 1 kg
        min_oxy_demand <- OxyDemand(Tpref = agi_dat2$Tpref_min, PO2_thresh = agi_dat2$thresh_min, T_C = min_temp_crop, 
                                W = weight, K = agi_dat2$K, Linf = agi_dat2$Linf, LwA = agi_dat2$LwA, LwB = agi_dat2$LwB)
        
        agi_min_depth <- min_o2_crop/min_oxy_demand

        #take median across time
        agi_min <- median(agi_min_depth, na.rm = TRUE)
        names(agi_min) <- "agi_min_depth"

    #agi med depth
    med_temp_crop <- crop(med_temp_rast, hull, mask = TRUE)
    med_o2_crop <- crop(med_o2_rast, hull, mask = TRUE)

        #calculate agi -- place holder weight of 1 kg
        med_oxy_demand <- OxyDemand(Tpref = agi_dat2$Tpref_med, PO2_thresh = agi_dat2$thresh_med, T_C = med_temp_crop, 
                                W = weight, K = agi_dat2$K, Linf = agi_dat2$Linf, LwA = agi_dat2$LwA, LwB = agi_dat2$LwB)
        
        agi_med_depth <- med_o2_crop/med_oxy_demand

        #take median across time
        agi_med <- median(agi_med_depth, na.rm = TRUE)
        names(agi_med) <- "agi_med_depth"

    #agi 75% quantile depth
    quant_temp_crop <- crop(quant_temp_rast, hull, mask = TRUE)
    quant_o2_crop <- crop(quant_o2_rast, hull, mask = TRUE)

        #calculate agi -- place holder weight of 1 kg
        quant_oxy_demand <- OxyDemand(Tpref = agi_dat2$Tpref_quant, PO2_thresh = agi_dat2$thresh_quant, T_C = quant_temp_crop, 
                                W = weight, K = agi_dat2$K, Linf = agi_dat2$Linf, LwA = agi_dat2$LwA, LwB = agi_dat2$LwB)
        
        agi_quant_depth <- quant_o2_crop/quant_oxy_demand

        #take median across time
        agi_quant <- median(agi_quant_depth, na.rm = TRUE)
        names(agi_quant) <- "agi_quant_depth"

    #combine agi
    agi_all <- c(agi_min, agi_med, agi_quant)
  }
}

#nwa species
  #load land mask
land <- ne_countries(scale = "large", returnclass = "sf")

  #get agi raster
agi_test <- AGI(sp_name = "Atlantic cod", weight = 8000, enviro = "bottom")

ggplot() +
  geom_spatraster(data = agi_test) +
  geom_sf(data = land, fill = "grey85", colour = "grey30", linewidth = 0.2) +
  coord_sf(xlim = as.vector(ext(agi_test))[1:2] + c(-2, 2),
           ylim = as.vector(ext(agi_test))[3:4] + c(-2, 2),
           expand = FALSE) +
  scale_fill_viridis_c(na.value = "transparent", name = "AGI", limits = c(0.5,2.5)) +
  labs(x = NULL, y = NULL) +
  theme_bw()
