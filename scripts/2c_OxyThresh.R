#libraries
library(tidyverse)
library(here)
library(terra)
library(sf)
source(here("functions/oxy_demand_functions_test.R"))

### load NWA data #####
#### bottom o2 -- no crop #####
# nwa_bo2 <- rast(here("data/enviro/nwa/do/raw/btm_o2.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))
# nwa_sob <- rast(here("data/enviro/nwa/salinity/raw/sob.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))
# nwa_tob <- rast(here("data/enviro/nwa/temp/raw/tob.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))
# nwa_bo2_atm <- rast_do_to_atm(do = nwa_bo2, s = nwa_sob, t = nwa_tob)

# writeCDF(nwa_bo2_atm, here("data/enviro/nwa/do/atm/nwa_bo2_atm.nc"), overwrite = TRUE)
# nwa_bo2_atm <- rast(here("data/enviro/nwa/do/atm/nwa_bo2_atm.nc"))

#     #filter for date range
#       target_dates <- time(nwa_bo2_atm) >= ym("1995-01") & time(nwa_bo2_atm) <= ym("2019-12")
#       nwa_bo2_sub <- nwa_bo2_atm[[target_dates]]

#    #calculate median across area for Tpref and update crs for cropping
#       med_bo2 <- median(nwa_bo2_sub, na.rm = TRUE)
#       writeCDF(med_bo2, here("data/enviro/nwa/do/processed/do_nwa_med.nc"), overwrite = TRUE)

med_bo2 <- rast(here("data/enviro/nwa/do/processed/do_nwa_med.nc"))

#### all do -- with union hull crop#####
nwa_o2 <- rast(here("data/enviro/nwa/do/processed/o2_nwa_crop.nc"))

#### all temp -- with union hull crop#####
nwa_temp <- rast(here("data/enviro/nwa/temp/processed/temp_nwa_crop.nc"))

#### all salinity -- with union hull crop#####
nwa_sal <- rast(here("data/enviro/nwa/salinity/processed/sal_nwa_crop.nc"))

#filter for date range (do for temp and salinity to convert later to atm)
target_dates <- time(nwa_o2) >= ym("1995-01") & time(nwa_o2) <= ym("2019-12")
nwa_o2_sub <- nwa_o2[[target_dates]]
nwa_temp_sub <- nwa_temp[[target_dates]]
nwa_sal_sub <- nwa_sal[[target_dates]]

### load NEP data #####
#### bottom DO #####
# nep_bo2 <- rast(here("data/enviro/nep/do/raw/btm_o2.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))
# nep_sob <- rast(here("data/enviro/nep/salinity/raw/sob.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))
# nep_tob <- rast(here("data/enviro/nep/temp/raw/tob.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))
# nep_bo2_atm <- rast_do_to_atm(do = nep_bo2, t = nep_tob, s = nep_sob)
#nep_bo2_atm_rot <- rotate(nep_bo2_atm)

#writeCDF(nep_bo2_atm_rot, here("data/enviro/nep/do/atm/nep_bo2_atm.nc"), overwrite = TRUE)
# #nep_bo2_atm <- rast(here("data/enviro/nep/do/atm/nep_bo2_atm.nc"))

#     #filter for date range
#       target_dates <- time(nep_bo2_atm) >= ym("1995-01") & time(nep_bo2_atm) <= ym("2019-12")
#       nep_bo2_sub <- nep_bo2_atm[[target_dates]]

#     #calculate median across area for Tpref and update crs for cropping
#       med_bo2 <- median(nep_bo2_sub, na.rm = TRUE)
#       writeCDF(med_bo2_rot, here("data/enviro/nep/do/processed/do_nep_med_rot.nc"), overwrite = TRUE)

med_bo2_rot <- rast(here("data/enviro/nep/do/processed/do_nep_med_rot.nc"))

#### all do -- with union hull crop#####
nep_o2 <- rast(here("data/enviro/nep/do/processed/o2_nep_crop.nc"))

#### all temp -- with union hull crop#####
nep_temp <- rast(here("data/enviro/nep/temp/processed/temp_nep_crop.nc"))

#### all salinity -- with union hull crop#####
nep_sal <- rast(here("data/enviro/nep/salinity/processed/sal_nep_crop.nc"))

 #filter for date range (do for temp and salinity to convert later to atm)
    target_dates <- time(nep_o2) >= ym("1995-01") & time(nep_o2) <= ym("2019-12")
    nep_o2_sub <- nep_o2[[target_dates]]
    nep_temp_sub <- nep_temp[[target_dates]]
    nep_sal_sub <- nep_sal[[target_dates]]
  
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

#### OxyThresh function ####
get_OxyThresh <- function(sp_dat, region){
 
  sp_dat$thresh_med <- NA
  sp_dat$thresh_min <- NA
  sp_dat$thresh_quant <- NA
  thresh_dat <- data.frame()

  for(i in 1:nrow(sp_dat)){ 
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
  
  if(region == "nwa" && enviro_layer == "bottom"){
    #filter for species range
      nwa_bo2_crop <- crop(med_bo2, hull, mask = TRUE)

    #take 10th percentile to get OxyThresh
      global_thresh <- terra::global(nwa_bo2_crop, quantile, probs = c(0.10), na.rm = TRUE)
      temp_dat$thresh_med = global_thresh[1,1]

  } else if(region == "nwa" && enviro_layer == "pelagic"){
     # get species do layers 
      min_o2_crop <- rast(here(paste0("data/enviro/nwa/do/atm/hull_crop/", curr_sp, "/min_depth/min_depth_o2.nc")))
      med_o2_crop <- rast(here(paste0("data/enviro/nwa/do/atm/hull_crop/", curr_sp, "/med_depth/med_depth_o2.nc")))
      quant_o2_crop <- rast(here(paste0("data/enviro/nwa/do/atm/hull_crop/", curr_sp, "/quant_depth/quant_depth_o2.nc")))
    
      #calculate median across area for Tpref, Tmin, Tquant
      #min depth OxyThresh
        #crop and take median
      min_o2_med <- median(min_o2_crop)
      
        #get 10th percentile
      min_global_thresh <- terra::global(min_o2_med, quantile, probs = c(0.10), na.rm = TRUE)
      temp_dat$thresh_min = min_global_thresh[1,1]
      
      #median depth OxyThresh
        #crop and take median
      med_o2_med <- median(med_o2_crop)

        #get 10th percentile
      med_global_thresh <- terra::global(med_o2_med, quantile, probs = c(0.10), na.rm = TRUE)
      temp_dat$thresh_med = med_global_thresh[1,1]

      #75% quantile depth OxyThresh
        #crop and take median
      quant_o2_med <- median(quant_o2_crop)

      #get 10th percentile
      quant_global_thresh <- terra::global(quant_o2_med, quantile, probs = c(0.10), na.rm = TRUE)
      temp_dat$thresh_quant = quant_global_thresh[1,1]
  
  } else if(region == "nep" && enviro_layer == "bottom"){
       #filter for species range
      nep_bo2_crop <- crop(med_bo2_rot, hull, mask = TRUE)

    #take 10th percentile to get OxyThresh
      global_thresh <- terra::global(nep_bo2_crop, quantile, probs = c(0.10), na.rm = TRUE)
      temp_dat$thresh_med = global_thresh[1,1]

  } else if(region == "nep" && enviro_layer == "pelagic"){
      # get species do layers 
      min_o2_crop <- rast(here(paste0("data/enviro/nep/do/atm/hull_crop/", curr_sp, "/min_depth/min_depth_o2.nc")))
      med_o2_crop <- rast(here(paste0("data/enviro/nep/do/atm/hull_crop/", curr_sp, "/med_depth/med_depth_o2.nc")))
      quant_o2_crop <- rast(here(paste0("data/enviro/nep/do/atm/hull_crop/", curr_sp, "/quant_depth/quant_depth_o2.nc")))

      #calculate median across area for Tpref, Tmin, Tquant
      #min depth OxyThresh
        #crop and take median
      min_o2_med <- median(min_o2_crop)
      
        #get 10th percentile
      min_global_thresh <- terra::global(min_o2_med, quantile, probs = c(0.10), na.rm = TRUE)
      temp_dat$thresh_min = min_global_thresh[1,1]
      
      #median depth OxyThresh
        #crop and take median
      med_o2_med <- median(med_o2_crop)

        #get 10th percentile
      med_global_thresh <- terra::global(med_o2_med, quantile, probs = c(0.10), na.rm = TRUE)
      temp_dat$thresh_med = med_global_thresh[1,1]

      #75% quantile depth OxyThresh
        #crop and take median
      quant_o2_med <- median(quant_o2_crop)

      #get 10th percentile
      quant_global_thresh <- terra::global(quant_o2_med, quantile, probs = c(0.10), na.rm = TRUE)
      temp_dat$thresh_quant = quant_global_thresh[1,1]
   
  }
  } 

  #save values
  thresh_dat <- rbind(thresh_dat, temp_dat)
  }

return(thresh_dat)

}

#Calculate OxyThresh
sp_dat_nwa <- sp_dat1 %>% filter(region == "nwa")
nwa_oxythresh <- get_OxyThresh(sp_dat = sp_dat_nwa, region = "nwa")
saveRDS(nwa_oxythresh, here("data/agi/nwa_oxythresh.rds"))

sp_dat_nep <- sp_dat1 %>% filter(region == "nep")
nep_oxythresh <- get_OxyThresh(sp_dat = sp_dat_nep, region = "nep")
saveRDS(nep_oxythresh, here("data/agi/nep_oxythresh.rds"))

all_oxythresh <- rbind(nwa_oxythresh, nep_oxythresh) %>% select(c(Common.name, thresh_med, thresh_min, thresh_quant))
all_oxythresh2 <- merge(sp_dat1, all_oxythresh, all.x = TRUE)

#save tpref data
saveRDS(all_oxythresh2, file = here("data/agi/sp_dat_oxythresh.rds"))

