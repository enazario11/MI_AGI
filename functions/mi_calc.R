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
MI <- function(sp_name, enviro, A0, E0, region){
  
      if(enviro == "bottom"){
        #load enviro data
        bo2_atm <- rast(here(paste0("data/enviro/", region ,"/do/atm/", region, "_bo2_atm.nc")))

        bto_folder <- list.files(here(paste0("data/enviro/", region, "/temp/processed/")), full.names = TRUE, pattern = "tob")
        bto <- rast(bto_folder[1])

        #get species hull
        sp_hull_path <- sp_hull[grepl(sp_name, sp_hull)]
        sp_hull_file <- list.files(sp_hull_path, full.names = TRUE, pattern = ".shp")
        sp_hull_file <- st_read(sp_hull_file)

        #crop enviro files to species hull
        bo2_crop <- crop(bo2_atm, sp_hull_file, mask = TRUE)
        bo2_kpa <- bo2_crop*101.325
        bto_crop <- crop(bto, sp_hull_file, mask = TRUE)
  
        #calculate mi
        mi <- MI_calc(A0 = A0, DO = bo2_kpa, E0 = E0, T_C = bto_crop)

        #take median across time
        mi_med <- median(mi, na.rm = TRUE)
        names(mi_med) <- paste(sp_name)
        
      return(mi_med)
    } #end of benthic 

    if (enviro == "pelagic"){ #end of benthic
      #load o2 and temp rast data per depth 
      min_temp <- rast(here(paste0("data/enviro/", region ,"/temp/processed/hull_crop/", sp_name, "/min_depth/min_depth_temp.nc")))
      min_o2_kpa <- rast(here(paste0("data/enviro/", region ,"/do/atm/hull_crop/", sp_name, "/min_depth/min_depth_o2.nc")))*101.325
      med_temp <- rast(here(paste0("data/enviro/", region ,"/temp/processed/hull_crop/", sp_name, "/med_depth/med_depth_temp.nc")))
      med_o2_kpa <- rast(here(paste0("data/enviro/", region ,"/do/atm/hull_crop/", sp_name, "/med_depth/med_depth_o2.nc")))*101.325
      quant_temp <- rast(here(paste0("data/enviro/", region ,"/temp/processed/hull_crop/", sp_name, "/quant_depth/quant_depth_temp.nc")))
      quant_o2_kpa <- rast(here(paste0("data/enviro/", region ,"/do/atm/hull_crop/", sp_name, "/quant_depth/quant_depth_o2.nc")))*101.325

      #mi min depth
      min_mi <- MI_calc(A0 = A0, DO = min_o2_kpa, E0 = E0, T_C = min_temp)
      
      #take median across time
      mi_min <- median(min_mi, na.rm = TRUE)
      names(mi_min) <- paste(sp_name, "-", "agi_min_depth")

      #mi med depth
      med_mi <- MI_calc(A0 = A0, DO = med_o2_kpa, E0 = E0, T_C = med_temp)
      
      #take median across time
      mi_med <- median(med_mi, na.rm = TRUE)
      names(mi_med) <- paste(sp_name, "-", "agi_med_depth")

      #mi 75% quantile depth
      quant_mi <- MI_calc(A0 = A0, DO = quant_o2_kpa, E0 = E0, T_C = quant_temp)

      #take median across time
      mi_quant <- median(quant_mi, na.rm = TRUE)
      names(mi_quant) <- paste(sp_name, "-", "agi_quant_depth")

    #combine mi
    mi_all <- c(mi_min, mi_med, mi_quant)
    
    return(mi_all)
      
    } #end of pelagic
  
} #end of function


