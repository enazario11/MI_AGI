#libraries
library(tidyverse)
library(here)
library(terra)
library(sf)
library(rnaturalearth)
library(tidyterra)
library(patchwork)
source(here("functions/oxy_demand_functions_test.R"))

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
               
        agi_all_loc <- rast() #empty raster for all loc x depth rasters
      
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

          #agi min depth
          min_o2_demand <- OxyDemand(Tpref = region_coef$Tpref_min[i], PO2_thresh = region_coef$thresh_min[i], T_C = min_temp, 
                                  W = weight, K = NULL, Linf = region_coef$Linf[i], LwA = region_coef$LwA[i], LwB = region_coef$LwB[i])
          
          agi_min_depth <- min_o2_atm/min_o2_demand

          #take median across time
          agi_min <- median(agi_min_depth, na.rm = TRUE)
          names(agi_min) <- paste(region_coef$region[i], "-", region_coef$species[i], "-", "agi_min_depth")

        #agi med depth
          #calculate agi
          med_o2_demand <- OxyDemand(Tpref = region_coef$Tpref_med[i], PO2_thresh = region_coef$thresh_med[i], T_C = med_temp, 
                                  W = weight, K = NULL, Linf = region_coef$Linf[i], LwA = region_coef$LwA[i], LwB = region_coef$LwB[i])
          
          agi_med_depth <- med_o2_atm/med_o2_demand

          #take median across time
          agi_med <- median(agi_med_depth, na.rm = TRUE)
          names(agi_med) <- paste(region_coef$region[i], "-", region_coef$species[i], "-", "agi_med_depth")

        #agi 75% quantile depth
          #calculate agi
          quant_o2_demand <- OxyDemand(Tpref = region_coef$Tpref_quant[i], PO2_thresh = region_coef$thresh_quant[i], T_C = quant_temp, 
                                  W = weight, K = NULL, Linf = region_coef$Linf[i], LwA = region_coef$LwA[i], LwB = region_coef$LwB[i])
          
          agi_quant_depth <- quant_o2_atm/quant_o2_demand

          #take median across time
          agi_quant <- median(agi_quant_depth, na.rm = TRUE)
          names(agi_quant) <- paste(region_coef$region[i], "-", region_coef$species[i], "-", "agi_quant_depth")

        #combine agi
        agi_all <- c(agi_min, agi_med, agi_quant)
        agi_all_loc <- c(agi_all_loc, agi_all)
        } #end of loc loop
       
        return(agi_all_loc)
      
    } #end of pelagic
  
} #end of function

#calc AGI crit
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

          agi_crit <- raster::clamp(agi[[i]], upper = agi_crit_pel_values[i, 1], values = FALSE)
          agi_crit <- as.polygons(agi_crit) %>% mutate(lyr = names(agi_crit))

          crit_pel_combo <- rbind(crit_pel_combo, agi_crit)

    } #end crit calc

  return(crit_pel_combo)
  } #end pelagic
} #end function