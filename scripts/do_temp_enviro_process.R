#### libraries #####
library(tidyverse)
library(here)
library(terra)
library(sf)
source(here("functions/oxy_demand_functions.R"))

### load NWA data #####
#### temperature #####
##### surface ####
nwa_stemp <- rast(here("data/enviro/nwa/temp/raw/tos.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

##### bottom ####
nwa_btemp <- rast(here("data/enviro/nwa/temp/raw/tob.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

#### salinity #####
nwa_sal <- rast(here("data/enviro/nwa/salinity/raw/so.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

#filter for bottom salinity
  #group by depth layer
z <- depth(nwa_sal)

  #calculate mean across time per depth layer
nwa_sal_avg <- tapp(nwa_sal, z, mean)

  #keep deepest cell with a non-NA value
last_non_na <- function(x) {
  if (all(is.na(x))) {
    return(NA) # Return NA if all layers are NA
  } else {
    non_na_idx <- which(!is.na(x))
    return(x[utils::tail(non_na_idx, 1)]) # Keep the last non-NA value
  }
}

nwa_sal_btm <- app(nwa_sal_avg, fun = last_non_na)

#### DO #####
##### surface ####
nwa_so2 <- rast(here("data/enviro/nwa/do/raw/o2_csurf.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

#convert to atm

##### bottom ####
nwa_bo2 <- rast(here("data/enviro/nwa/do/raw/btm_o2.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

#convert to atm

### load NEP data #####
#### temperature #####
##### surface ####
nep_stemp <- rast(here("data/enviro/nep/temp/raw/tos.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))

##### bottom ####
#nep_btemp <- rast(here("data/enviro/nep/temp/raw/tob.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))
#nep_btemp <- rotate(nep_btemp)
#writeCDF(nep_btemp, here("data/enviro/nep/temp/nep_btemp_rot.nc"))

nep_btemp <- rast(here("data/enviro/nep/temp/nep_btemp_rot.nc"))

#### salinity #####
#filter for bottom salinity

#### DO #####
##### surface ####
nep_so2 <- rast(here("data/enviro/nep/do/raw/o2_csurf.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))

#convert to atm

##### bottom ####
nep_bo2 <- rast(here("data/enviro/nep/do/raw/btm_o2.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))

#convert to atm


### Tpref ######
sp_dat <- read.csv(here("data/fishglob/glob_metdat.csv"))

dat_glob <- readRDS(here("data/fishglob/fishglob_usa.rds")) %>% 
  filter(survey == "NEUS" | survey == "SEUS" | survey == "WCANN" | survey == "WCTRI") %>%
  filter(year >= 1993) 

get_Tpref <- function(sp_dat, temp_dat, region){
  
  sp_dat$Tpref <- NA
  tpref_dat <- data.frame()

  for(i in 1:nrow(sp_dat)){
  curr_sp = sp_dat$Common.name[i]
  temp_dat = sp_dat %>% filter(Common.name == curr_sp)

  curr_glob = dat_glob %>% filter(accepted_name == paste(temp_dat$Genus, temp_dat$Species))

  if(nrow(curr_glob) > 1){  
      lat_max <- max(curr_glob$latitude) + 2
      lat_min <- min(curr_glob$latitude) - 2
      lon_max <- max(curr_glob$longitude) +2
      lon_min <- min(curr_glob$longitude) -2
  
      range_box <- ext(lon_min, lon_max, lat_min, lat_max)
  
      curr_glob$date <- paste0(curr_glob$year,"-", curr_glob$month)
      curr_glob$date <- ym(curr_glob$date)
      survey_start <- min(curr_glob$date)
      survey_end <- max(curr_glob$date)
  
  if(region == "nwa"){
              
            #filter for date range
              target_dates <- time(nwa_btemp) >= survey_start & time(nwa_btemp) <= survey_end
              nwa_btemp_sub <- nwa_btemp[[target_dates]]
            
            #filter for species range
              nwa_btemp_crop <- crop(nwa_btemp_sub, range_box)
    
            #calculate median across area for Tpref
              med_temp <- median(nwa_btemp_crop)
              global_avg <- terra::global(med_temp, fun = "mean", na.rm = TRUE)
            
              temp_dat$Tpref = global_avg[1,1]

  } else if(region == "nep"){
      
    #filter for date range
      target_dates <- time(nep_btemp) >= survey_start & time(nep_btemp) <= survey_end
      nep_btemp_sub <- nep_btemp[[target_dates]]
    
    #filter for species range
      nep_btemp_crop <- crop(nep_btemp_sub, range_box)

    #calculate median across area for Tpref
      med_temp <- median(nep_btemp_crop)
      global_avg <- terra::global(med_temp, fun = "mean", na.rm = TRUE)
    
      temp_dat$Tpref = global_avg[1,1]

  }
  } 

  #save values
  tpref_dat <- rbind(tpref_dat, temp_dat)
}

return(tpref_dat)

}

#simplify input data
sp_dat_nwa<- sp_dat %>% filter(enviro_layer == "bottom" & region == "nwa")
nwa_tpref <- get_Tpref(sp_dat = sp_dat_nwa, temp_dat = nwa_btemp, region = "nwa")

sp_dat_nep <- sp_dat %>% filter(enviro_layer == "bottom" & region == "nep")
nep_tpref <- get_Tpref(sp_dat = sp_dat_nep, temp_dat = nep_btemp, region = "nep")

all_tpref <- rbind(nwa_tpref, nep_tpref) %>% select(c(Common.name, Tpref))
all_tpref2 <- merge(sp_dat, all_tpref, all.x = TRUE)
#saveRDS(all_tpref2, file = here("data/agi/sp_dat_tpef.rds"))

### OxyThresh #####
