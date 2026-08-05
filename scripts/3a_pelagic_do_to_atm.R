#libraries
library(tidyverse)
library(here)
library(terra)
library(sf)
library(rnaturalearth)
library(tidyterra)
library(patchwork)
source(here("functions/oxy_demand_functions_test.R"))

#convert DO to atm across cropped rasters, save hulls
#extract data hulls
write_hulls <- function(sp_dat, oxy_path, temp_path, sal_path){

  for(i in 2:nrow(sp_dat)) { 
     oxy = rast(here(oxy_path))
     temp = rast(here(temp_path))
     sal = rast(here(sal_path))

    curr_sp = sp_dat$Common.name[i]
    print(curr_sp)
    temp_dat = sp_dat %>% filter(Common.name == curr_sp)
    enviro_layer = temp_dat$enviro_layer

    #filter for current species and remove survey rows w/ 0 counts
    curr_glob <- dat_glob %>% 
      filter(accepted_name == paste(temp_dat$Genus, temp_dat$Species)) 
    
    #filter for survey rows where more than the 25% quantile were pulled to make sure it's "true" habitat
    low_quant_num <- quantile(curr_glob$num, 0.25)
    curr_glob <- curr_glob %>%
      filter(num > low_quant_num)
    
    if(nrow(curr_glob) > 1){  
        #create bounding polygon to crop raster with -- did not save bc already did with Tpref function
        pts <- vect(curr_glob, geom = c("longitude", "latitude"), crs = "EPSG:4326")
        hull <- convHull(pts)
      
        # get right depth layers
        min_layer <- which.min(abs(depth(oxy) - temp_dat$min_depth))
        min_seq <- seq(from = min_layer, to = nlyr(oxy), by = length(unique(depth(oxy))))

        med_layer <- which.min(abs(depth(oxy) - temp_dat$med_depth))
        med_seq <- seq(from = med_layer, to = nlyr(oxy), by = length(unique(depth(oxy))))
      
        quant_layer <- which.min(abs(depth(oxy) - temp_dat$quant_depth))
        quant_seq <- seq(from = quant_layer, to = nlyr(oxy), by = length(unique(depth(oxy))))
      
        min_o2_rast <- oxy[[min_seq]]
        min_temp_rast <- temp[[min_seq]]
        min_sal_rast <- sal[[min_seq]]
      
        med_o2_rast <- oxy[[med_seq]]
        med_temp_rast <- temp[[med_seq]]
        med_sal_rast <- sal[[med_seq]]
      
        quant_o2_rast <- oxy[[quant_seq]]
        quant_temp_rast <- temp[[quant_seq]]
        quant_sal_rast <- sal[[quant_seq]]
      
        #crop and convert do to atm
        min_o2_atm <- rast_do_to_atm(do = min_o2_rast, t = min_temp_rast, s = min_sal_rast)
        med_o2_atm <- rast_do_to_atm(do = med_o2_rast, t = med_temp_rast, s = med_sal_rast)
        quant_o2_atm <- rast_do_to_atm(do = quant_o2_rast, t = quant_temp_rast, s = quant_sal_rast)
        
        #min depth
        min_o2_crop <- crop(min_o2_atm, hull, mask = TRUE)
        dir.create(paste0("data/enviro/", temp_dat$region ,"/do/atm/hull_crop/", curr_sp, "/min_depth"), recursive = TRUE, showWarnings = FALSE)
        writeCDF(min_o2_crop, paste0("data/enviro/", temp_dat$region ,"/do/atm/hull_crop/", curr_sp, "/min_depth/min_depth_o2.nc"))
      
        min_temp_crop <- crop(min_temp_rast, hull, mask = TRUE)
        dir.create(paste0("data/enviro/", temp_dat$region ,"/temp/processed/hull_crop/", curr_sp, "/min_depth"), recursive = TRUE, showWarnings = FALSE)
        writeCDF(min_temp_crop, paste0("data/enviro/", temp_dat$region ,"/temp/processed/hull_crop/", curr_sp, "/min_depth/min_depth_temp.nc"))

        #median depth
        med_o2_crop <- crop(med_o2_atm, hull, mask = TRUE)
        dir.create(paste0("data/enviro/", temp_dat$region ,"/do/atm/hull_crop/", curr_sp, "/med_depth"), recursive = TRUE, showWarnings = FALSE)
        writeCDF(med_o2_crop, paste0("data/enviro/", temp_dat$region ,"/do/atm/hull_crop/", curr_sp, "/med_depth/med_depth_o2.nc"))
      
        med_temp_crop <- crop(med_temp_rast, hull, mask = TRUE)
        dir.create(paste0("data/enviro/", temp_dat$region ,"/temp/processed/hull_crop/", curr_sp, "/med_depth"), recursive = TRUE, showWarnings = FALSE)
        writeCDF(med_temp_crop, paste0("data/enviro/", temp_dat$region ,"/temp/processed/hull_crop/", curr_sp, "/med_depth/med_depth_temp.nc"))
      
        #75% quantile depth
        quant_o2_crop <- crop(quant_o2_atm, hull, mask = TRUE)
        dir.create(paste0("data/enviro/", temp_dat$region ,"/do/atm/hull_crop/", curr_sp, "/quant_depth"), recursive = TRUE, showWarnings = FALSE)
        writeCDF(quant_o2_crop, paste0("data/enviro/", temp_dat$region ,"/do/atm/hull_crop/", curr_sp, "/quant_depth/quant_depth_o2.nc"))
      
        quant_temp_crop <- crop(quant_temp_rast, hull, mask = TRUE)
        dir.create(paste0("data/enviro/", temp_dat$region ,"/temp/processed/hull_crop/", curr_sp, "/quant_depth"), recursive = TRUE, showWarnings = FALSE)
        writeCDF(quant_temp_crop, paste0("data/enviro/", temp_dat$region ,"/temp/processed/hull_crop/", curr_sp, "/quant_depth/quant_depth_temp.nc"))
    
    } #end of hull crop
  } 
} #end of function

### fishglob survey data #####
sp_dat1 <- read.csv(here("data/fishglob/glob_metdat.csv"))

load(here("data/fishglob/FishGlob_public_clean.RData"))
dat_glob <- data
dat_glob <- dat_glob %>% #readRDS(here("data/fishglob/fishglob_usa.rds"))
  filter(country == "United States") %>%
  filter(survey == "NEUS" | survey == "SEUS" | survey == "WCANN" | survey == "WCTRI") %>%
  filter(year >= 1993 & num > 0) %>% 
  filter(accepted_name != "Scomber japonicus" | #pacific mackeral somehow had locs from NEUS/SEUS surveys?
        (accepted_name == "Scomber japonicus" & survey %in% c("WCANN", "WCTRI")))

#generate hulls
sp_dat_nwa_p <- sp_dat1 %>% filter(region == "nwa" & enviro_layer == "pelagic")
sp_dat_nep_p <- sp_dat1 %>% filter(region == "nep" & enviro_layer == "pelagic")

#run hull crop function
write_hulls(sp_dat = sp_dat_nwa_p, 
            oxy_path = "data/enviro/nwa/do/processed/o2_nwa_crop.nc", 
            temp_path = "data/enviro/nwa/temp/processed/temp_nwa_crop.nc", 
            sal_path = "data/enviro/nwa/salinity/processed/sal_nwa_crop.nc")

write_hulls(sp_dat = sp_dat_nep_p, 
            oxy_path = "data/enviro/nep/do/processed/o2_nep_crop.nc",
            temp_path = "data/enviro/nep/temp/processed/temp_nep_crop.nc", 
            sal_path = "data/enviro/nep/salinity/processed/sal_nep_crop.nc")
