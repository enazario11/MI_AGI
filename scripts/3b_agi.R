#libraries
library(tidyverse)
library(here)
library(terra)
library(sf)
library(rnaturalearth)
library(tidyterra)
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
AGI <- function(sp_name, weight = NULL, sex, season, enviro){ #consider adding enviro dat paths as arguments
  
  #isolate to species of interest
  agi_dat2 <- agi_dat %>% filter(Common.name == sp_name)

      if(enviro == "bottom"){
        #load enviro data
        bo2_atm <- rast(here(paste0("data/enviro/", agi_dat2$region ,"/do/atm/", agi_dat2$region, "_bo2_atm.nc")))

        bto_folder <- list.files(here(paste0("data/enviro/", agi_dat2$region, "/temp/processed/")), full.names = TRUE, pattern = "tob")
        bto <- rast(bto_folder[1])

        #get species hull
        sp_hull_path <- sp_hull[grepl(sp_name, sp_hull)]
        sp_hull_file <- st_read(paste0(sp_hull_path, "/", agi_dat2$Genus[1], " ", agi_dat2$Species[1], ".shp"))

        #crop enviro files to species hull
        bo2_crop <- crop(bo2_atm, sp_hull_file, mask = TRUE)
        bto_crop <- crop(bto, sp_hull_file, mask = TRUE)
  
      #get region specific agi coefs
        region_coef <- data.frame(
            region = character(length(unique(agi_dat2$Location))),
            species = agi_dat2$Common.name[1],
            Tpref = mean(agi_dat2$Tpref_med),
            thresh_med = mean(agi_dat2$thresh_med),
            LwA = double(length(unique(agi_dat2$Location))), 
            LwB = double(length(unique(agi_dat2$Location))), 
            Linf = double(length(unique(agi_dat2$Location)))
          )
      
        for(i in 1:length(unique(agi_dat2$Location))){
          curr_loc <- unique(agi_dat2$Location)[i] 
          loc_dat <- agi_dat2 %>% filter(Location == curr_loc)
          region_coef$region[i] <- curr_loc

          if(length(unique(loc_dat$Sex.1)) > 1 | length(unique(loc_dat$Season > 1))){
            Linf <- mean(loc_dat$Linf, na.rm = TRUE)
            LwA <- mean(loc_dat$LwA, na.rm = TRUE)
            LwB <- mean(loc_dat$LwB, na.rm = TRUE)

            region_coef$Linf[i] <- Linf
            region_coef$LwA[i] <- LwA
            region_coef$LwB[i] <- LwB
          }
        }

        #calculate agi
        agi_all <- rast()
        for(i in 1:nrow(region_coef)){
          oxy_demand <- OxyDemand(Tpref = region_coef$Tpref[i], PO2_thresh = region_coef$thresh_med[i], T_C = bto_crop, 
                                  W = weight, K = NULL, Linf = region_coef$Linf[i], LwA = region_coef$LwA[i], LwB = region_coef$LwB[i])
        
          agi <- bo2_crop/oxy_demand

          #take median across time
          agi_med <- median(agi, na.rm = TRUE)
          names(agi_med) <- paste(region_coef$region[i], "-", region_coef$species[i])
          
          agi_all <- c(agi_all, agi_med)
        }
  
      return(agi_all)
    } #end of benthic 

    if (enviro == "pelagic"){ #end of benthic
        #load o2 and temp rast data per depth 
        min_temp <- rast(here(paste0("data/enviro/", agi_dat2$region ,"/temp/processed/hull_crop/", sp_name, "/min_depth/min_depth_temp.nc")))
        min_o2_atm <- rast(here(paste0("data/enviro/", agi_dat2$region ,"/do/atm/hull_crop/", sp_name, "/min_depth/min_depth_o2.nc")))
        med_temp <- rast(here(paste0("data/enviro/", agi_dat2$region ,"/temp/processed/hull_crop/", sp_name, "/med_depth/med_depth_temp.nc")))
        med_o2_atm <- rast(here(paste0("data/enviro/", agi_dat2$region ,"/do/atm/hull_crop/", sp_name, "/med_depth/med_depth_o2.nc")))
        quant_temp <- rast(here(paste0("data/enviro/", agi_dat2$region ,"/temp/processed/hull_crop/", sp_name, "/quant_depth/quant_depth_temp.nc")))
        quant_o2_atm <- rast(here(paste0("data/enviro/", agi_dat2$region ,"/do/atm/hull_crop/", sp_name, "/quant_depth/quant_depth_o2.nc")))

        #get region specific agi coefs
        region_coef <- data.frame(
            region = character(length(unique(agi_dat2$Location))),
            species = agi_dat2$Common.name[1],
            Tpref_min = mean(agi_dat2$Tpref_min),
            Tpref_med = mean(agi_dat2$Tpref_med), 
            Tpref_quant = mean(agi_dat2$Tpref_quant),
            thresh_min = mean(agi_dat2$thresh_min), 
            thresh_med = mean(agi_dat2$thresh_med),
            thresh_quant = mean(agi_dat2$thresh_quant),
            LwA = double(length(unique(agi_dat2$Location))), 
            LwB = double(length(unique(agi_dat2$Location))), 
            Linf = double(length(unique(agi_dat2$Location)))
          )
      
      #START HERE
        for(i in 1:length(unique(agi_dat2$Location))){
          curr_loc <- unique(agi_dat2$Location)[i] 
          loc_dat <- agi_dat2 %>% filter(Location == curr_loc)
          region_coef$region[i] <- curr_loc

          if(length(unique(loc_dat$Sex.1)) > 1 | length(unique(loc_dat$Season > 1))){
            Linf <- mean(loc_dat$Linf, na.rm = TRUE)
            LwA <- mean(loc_dat$LwA, na.rm = TRUE)
            LwB <- mean(loc_dat$LwB, na.rm = TRUE)

            region_coef$Linf[i] <- Linf
            region_coef$LwA[i] <- LwA
            region_coef$LwB[i] <- LwB
          }
        }

        #agi min depth
          #calculate agi
          min_o2_demand <- OxyDemand(Tpref = agi_dat2$Tpref_min, PO2_thresh = agi_dat2$thresh_min, T_C = min_temp, 
                                  W = weight, K = NULL, Linf = agi_dat2$Linf, LwA = agi_dat2$LwA, LwB = agi_dat2$LwB)
          
          agi_min_depth <- min_o2_atm/min_o2_demand

          #take median across time
          agi_min <- median(agi_min_depth, na.rm = TRUE)
          names(agi_min) <- "agi_min_depth"

        #agi med depth
          #calculate agi
          med_o2_demand <- OxyDemand(Tpref = agi_dat2$Tpref_med, PO2_thresh = agi_dat2$thresh_med, T_C = med_temp, 
                                  W = weight, K = NULL, Linf = agi_dat2$Linf, LwA = agi_dat2$LwA, LwB = agi_dat2$LwB)
          
          agi_med_depth <- med_o2_atm/med_o2_demand

          #take median across time
          agi_med <- median(agi_med_depth, na.rm = TRUE)
          names(agi_med) <- "agi_med_depth"

        #agi 75% quantile depth
          #calculate agi
          quant_o2_demand <- OxyDemand(Tpref = agi_dat2$Tpref_quant, PO2_thresh = agi_dat2$thresh_quant, T_C = quant_temp, 
                                  W = weight, K = NULL, Linf = agi_dat2$Linf, LwA = agi_dat2$LwA, LwB = agi_dat2$LwB)
          
          agi_quant_depth <- quant_o2_atm/quant_o2_demand

          #take median across time
          agi_quant <- median(agi_quant_depth, na.rm = TRUE)
          names(agi_quant) <- "agi_quant_depth"

        #combine agi
        agi_all <- c(agi_min, agi_med, agi_quant)
        return(agi_all)
      
    } #end of pelagic
  
} #end of function

get_crit <- function(agi, enviro){
  if(enviro == "bottom"){
    crit_bot_combo <- vect()

    for(i in 1:nlyr(agi)){
    curr_lyr <- agi[[i]]
      
    agi_crit_bot <- raster::clamp(agi, upper = quantile(values(agi), 0.10, na.rm = TRUE), values = FALSE)
    agi_crit_bot <- as.polygons(agi_crit_bot) %>% mutate(lyr = names(curr_lyr))
    
    crit_bot_combo <- rbind(crit_bot_combo, agi_crit_bot)
    } #end crit calc
    
    return(crit_bot_combo)   

  } else if(enviro == "pelagic") { #end bottom
    crit_pel_combo <- vect()

    for(i in 1:nlyr(agi)){

      curr_lyr <- agi[[i]]

        #agi crit per layer
          agi_crit_pel_values <- global(agi, fun = quantile, probs = c(0.10), na.rm = TRUE) #calc crit per depth layer

          agi_crit_min <- raster::clamp(agi[[1]], upper = agi_crii_values[1,], values = FALSE)
          agi_crit_min <- as.polygons(agi_crit_min) %>% mutate(lyr = "agi_min_depth")


          agi_crit_med <- raster::clamp(agi[[2]], upper = agi_crii_values[2,], values = FALSE)
          agi_crit_med <- as.polygons(agi_crit_med) %>% mutate(lyr = "agi_med_depth")


          agi_crit_quant <- raster::clamp(agi[[3]], upper = agi_crii_values[3,], values = FALSE)
          agi_crit_quant <- as.polygons(agi_crit_quant) %>% mutate(lyr = "agi_quant_depth")


          crit_combo <- rbind(agi_crit_min, agi_crit_med, agi_crit_quant)
          return(crit_combo)

    } #end crit calc
  } #end pelagic
} #end function

#plot median agi values across survey period -- not specifying agi values for sex, season, or region
  #load land mask
land <- ne_countries(scale = "large", returnclass = "sf")

  #agi raster bottom species
agi_bot <- AGI(sp_name = "Acadian redfish", enviro = "bottom")
agi_crit_bot <- get_crit(agi = agi_bot, enviro = "bottom")

ggplot() +
  geom_spatraster(data = agi_bot) +
  geom_spatvector(data = agi_crit_bot, color = "black", fill = NA, linewidth = 1) +
  #geom_spatvector(data = agi_one_bot, color = "grey15", fill = NA, linewidth = 1) +
  geom_sf(data = land, fill = "grey85", colour = "grey30", linewidth = 0.2) +
  coord_sf(xlim = as.vector(ext(agi_bot))[1:2] + c(-2, 2),
           ylim = as.vector(ext(agi_bot))[3:4] + c(-2, 2),
           expand = FALSE) +
  scale_fill_whitebox_c(palette = "muted", direction = -1) +
  facet_wrap(~lyr) +
  labs(x = NULL, y = NULL) +
  tidyquant::theme_tq() + 
  theme(strip.text = element_text(size = 14))
  
  #agi raster pelagic species
agi_pel <- AGI(sp_name = "Pacific sardine", enviro = "pelagic")

agi_crit_pel <- get_crit_pel(agi_pel)
agi_one_pel <- get_one_pel(agi_pel)

ggplot() +
  geom_spatraster(data = agi_pel) +
  geom_spatvector(data = agi_crit_pel, color = "black", fill = NA, linewidth = 1) +
  geom_spatvector(data = agi_one_pel, color = "grey15", fill = NA, linewidth = 1) +
  geom_sf(data = land, fill = "grey85", colour = "grey30", linewidth = 0.2) +
  coord_sf(xlim = as.vector(ext(agi_pel))[1:2] + c(-2, 2),
           ylim = as.vector(ext(agi_pel))[3:4] + c(-2, 2),
           expand = FALSE) +
  scale_fill_whitebox_c(palette = "muted", direction = -1) +
  labs(x = NULL, y = NULL, fill = "AGI") +
  theme_minimal() + 
  facet_wrap(~lyr) +
  theme(strip.text = element_text(size = 16))
  