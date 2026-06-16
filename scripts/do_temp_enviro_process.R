#### libraries #####
library(tidyverse)
library(here)
library(terra)
library(sf)
source(here("functions/oxy_demand_functions.R"))

### load NWA data #####
#### temperature #####
nwa_btemp <- rast(here("data/enviro/nwa/temp/raw/tob.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

#### salinity #####
nwa_sal <- rast(here("data/enviro/nwa/salinity/raw/sob.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

#### DO #####
nwa_bo2 <- rast(here("data/enviro/nwa/do/raw/btm_o2.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

#convert to atm


### load NEP data #####
#### temperature #####

  #nep_btemp <- rast(here("data/enviro/nep/temp/raw/tob.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))
  #nep_btemp <- rotate(nep_btemp)
  #writeCDF(nep_btemp, here("data/enviro/nep/temp/nep_btemp_rot.nc"))

nep_btemp <- rast(here("data/enviro/nep/temp/nep_btemp_rot.nc"))

#### salinity #####
nep_sal <- rast(here("data/enviro/nep/salinity/raw/sob.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))



#### DO #####
nep_bo2 <- rast(here("data/enviro/nep/do/raw/btm_o2.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))

#convert to atm



### Tpref ######
sp_dat <- read.csv(here("data/fishglob/glob_metdat.csv"))

dat_glob <- readRDS(here("data/fishglob/fishglob_usa.rds")) %>% 
  filter(survey == "NEUS" | survey == "SEUS" | survey == "WCANN" | survey == "WCTRI") %>%
  filter(year >= 1993 & num > 0) 

get_Tpref <- function(sp_dat, temp_dat, region){
  
  sp_dat$Tpref_med <- NA
  tpref_dat <- data.frame()

  for(i in 1:nrow(sp_dat)){ 
  curr_sp = sp_dat$Common.name[i]
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

      #create bounding polygon to crop raster with
      pts <- vect(curr_glob, geom = c("longitude", "latitude"), crs = "EPSG:4326")
      hull <- convHull(pts)
    
      #save shapefile and plot of convex hull
      dir.create(here(paste0("data/fish_hull/", curr_sp)))
      writeVector(hull, here(paste0("data/fish_hull/",curr_sp,"/", curr_glob$accepted_name, ".shp")))
    
      png(paste0("data/fish_hull/", curr_sp, "/", curr_sp, ".png"), width = 700, height = 700, res = 120)
      plot(hull)
      plot(pts, add = TRUE, col = "cornflowerblue", main = curr_sp)
      dev.off()

      #set date time column to subset 
      curr_glob$date <- paste0(curr_glob$year,"-", curr_glob$month)
      curr_glob$date <- ym(curr_glob$date)
  
  if(region == "nwa" & enviro_layer == "bottom"){
              
            #filter for date range
              target_dates <- time(nwa_btemp) >= ym("1995-01") & time(nwa_btemp) <= ym("2019-12")
              nwa_btemp_sub <- nwa_btemp[[target_dates]]
            
            #filter for species range
               nwa_btemp_crop <- crop(nwa_btemp_sub, hull)
               nwa_btemp_mask <- mask(nwa_btemp_crop, hull)
    
            #calculate median across area for Tpref
              med_temp <- median(nwa_btemp_mask)
              global_avg <- terra::global(med_temp, fun = "mean", na.rm = TRUE)
            
              temp_dat$Tpref_med = global_avg[1,1]

  } else if(region == "nwa" & enviro_layer == "pelagic"){
      
    #filter for date range
      target_dates <- time(nwa_btemp) >= ym("1995-01") & time(nwa_btemp) <= ym("2019-12")
      nwa_btemp_sub <- nwa_btemp[[target_dates]]
    
    
    
    

  } else if(region == "nep" & enviro_layer == "bottom"){
      
    #filter for date range
      target_dates <- time(nep_btemp) >= ym("1995-01") & time(nep_btemp) <= ym("2019-12")
      nep_btemp_sub <- nep_btemp[[target_dates]]
    
    #filter for species range
      nep_btemp_crop <- crop(nep_btemp_sub, hull)
      nep_btemp_mask <- mask(nep_btemp_crop, hull)

    #calculate median across area for Tpref
      med_temp <- median(nep_btemp_mask)
      global_avg <- terra::global(med_temp, fun = "mean", na.rm = TRUE)
    
      temp_dat$Tpref_med = global_avg[1,1]

  } else if(region == "nep" & enviro_layer == "pelagic"){
      
    #filter for date range
      target_dates <- time(nep_btemp) >= ym("1995-01") & time(nep_btemp) <= ym("2019-12")
      nep_btemp_sub <- nep_btemp[[target_dates]]
    
    
    

  }
  } 

  #save values
  tpref_dat <- rbind(tpref_dat, temp_dat)
  }

return(tpref_dat)

}

#simplify input data
sp_dat_nwa <- sp_dat %>% filter(region == "nwa")
nwa_tpref <- get_Tpref(sp_dat = sp_dat_nwa, temp_dat = nwa_btemp, region = "nwa")

sp_dat_nep <- sp_dat %>% filter(region == "nep")
nep_tpref <- get_Tpref(sp_dat = sp_dat_nep, temp_dat = nep_btemp, region = "nep")

all_tpref <- rbind(nwa_tpref, nep_tpref) %>% select(c(Common.name, Tpref))
all_tpref2 <- merge(sp_dat, all_tpref, all.x = TRUE)

#save tpref data
saveRDS(all_tpref2, file = here("data/agi/sp_dat_tpef.rds"))

### OxyThresh #####
sp_dat_tpref <- readRDS(here("data/agi/sp_dat_tpef.rds"))




