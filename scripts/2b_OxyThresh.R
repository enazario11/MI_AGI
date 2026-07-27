#libraries
library(tidyverse)
library(here)
library(terra)
library(sf)
source(here("functions/oxy_demand_functions.R"))

### load NWA data #####
#### bottom salinity #####
nwa_bsal <- rast(here("data/enviro/nwa/salinity/raw/sob.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

#### all z salinity #####
nwa_sal <- rast(here("data/enviro/nwa/salinity/raw/so.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

#### bottom DO #####
#nwa_bo2 <- rast(here("data/enviro/nwa/do/raw/btm_o2.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

#convert to atm
#nwa_bo2_atm <- do_to_atm(do = nwa_bo2, t = nwa_btemp, s = nwa_bsal, thresh = FALSE)
#writeCDF(nwa_bo2_atm, here("data/enviro/nwa/do/nwa_bo2_atm.nc"))

nwa_bo2_atm <- rast(here("data/enviro/nwa/do/atm/nwa_bo2_atm.nc"))

#### all z do #####
nwa_o2 <- rast(here("data/enviro/nwa/do/raw/o2.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

### load NEP data #####
#### bottom salinity #####
nep_bsal <- rast(here("data/enviro/nep/salinity/raw/sob.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))

#### all z salinity #####
nep_sal <- rast(here("data/enviro/nep/salinity/raw/so.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))

#### bottom DO #####
# nep_bo2 <- rast(here("data/enviro/nep/do/raw/btm_o2.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))

# #convert to atm
# nep_bo2_atm <- do_to_atm(do = nep_bo2, t = nep_btemp, s = nep_bsal, thresh = FALSE)
# writeCDF(nep_bo2_atm, here("data/enviro/nep/do/nep_bo2_atm.nc"))

nep_bo2_atm <- rast(here("data/enviro/nep/do/atm/nep_bo2_atm.nc"))

#### all z do #####
nep_o2 <- rast(here("data/enviro/nep/do/raw/o2.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))

### fishglob survey data ######
sp_dat <- read.csv(here("data/fishglob/glob_metdat.csv"))

load(here("data/fishglob/FishGlob_public_clean.RData"))
dat_glob <- data
dat_glob <- dat_glob %>% #readRDS(here("data/fishglob/fishglob_usa.rds"))
  filter(country == "United States") %>%
  filter(survey == "NEUS" | survey == "SEUS" | survey == "WCANN" | survey == "WCTRI") %>%
  filter(year >= 1993 & num > 0) %>% 
  filter(accepted_name != "Scomber japonicus" | #pacific mackeral somehow had locs from NEUS/SEUS surveys?
        (accepted_name == "Scomber japonicus" & survey %in% c("WCANN", "WCTRI")))


#### OxyThresh function ####
### OxyThresh #####
#nwa union hull
nwa_glob <- dat_glob %>% filter(survey == "NEUS" | survey == "SEUS")

nwa_pts <- st_as_sf(
  nwa_glob,
  coords = c("longitude", "latitude"),
  crs = crs(nwa_o2),
  remove = FALSE
)

nwa_hulls <- nwa_pts %>%
  group_by(accepted_name) %>%
  summarize(geometry = st_combine(geometry), .groups = "drop") %>%
  st_convex_hull()

nwa_union <- st_union(nwa_hulls)

  #save for python processing to get crop file (crop_enviro_zarr.py file)
nwa_union_sf <- st_sf(region = "nwa", geometry = nwa_union)
st_write(nwa_union_sf, "data/enviro/nwa/nwa_union.gpkg", delete_dsn = TRUE)

#nep union hull
nep_glob <- dat_glob %>% filter(survey == "WCANN" | survey == "WCTRI")

nep_pts <- st_as_sf(
  nep_glob,
  coords = c("longitude", "latitude"),
  crs = crs(nep_o2),
  remove = FALSE
)

nep_hulls <- nep_pts %>%
  group_by(accepted_name) %>%
  summarize(geometry = st_combine(geometry), .groups = "drop") %>%
  st_convex_hull()

nep_union <- st_union(nep_hulls)

  #save for python processing to get crop file (crop_enviro_zarr.py file)
nep_union_sf <- st_sf(region = "nep", geometry = nep_union)
st_write(nep_union_sf, "data/enviro/nep/nep_union.gpkg", delete_dsn = TRUE)

#Load cropped rasters and convert DO to atm 
nwa_o2_crop <- rast(here("data/enviro/nwa/do/processed/o2_nwa_crop.nc"))
nwa_temp_crop <- rast(here("data/enviro/nwa/temp/processed/temp_nwa_crop.nc"))
nwa_sal_crop <- rast(here("data/enviro/nwa/salinity/processed/sal_nwa_crop.nc"))

nwa_do_atm <- do_to_atm(do = nwa_o2_crop, t = nwa_temp_crop, s = nwa_sal_crop)
units(nwa_do_atm) <- "atm"
writeCDF(nwa_do_atm, here("data/enviro/nwa/do/atm/nwa_o2_atm.nc"))

#nep
nep_o2_crop <- rast(here("data/enviro/nep/do/processed/o2_nep_crop.nc"))
nep_temp_crop <- rast(here("data/enviro/nep/temp/processed/temp_nep_crop.nc"))
nep_sal_crop <- rast(here("data/enviro/nep/salinity/processed/sal_nep_crop.nc"))

nep_do_atm <- do_to_atm(do = nep_o2_crop, t = nep_temp_crop, s = nep_sal_crop)
units(nep_do_atm) <- "atm"
writeCDF(nep_do_atm, here("data/enviro/nep/do/atm/nep_o2_atm.nc"))

#get oxythresh values across species
nwa_o2_atm <- rast(here("data/enviro/nwa/do/atm/nwa_o2_atm.nc"))
nep_o2_atm <- rast(here("data/enviro/nep/do/atm/nep_o2_atm.nc"))

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
    
      #set date time column to subset 
      curr_glob$date <- paste0(curr_glob$year,"-", curr_glob$month)
      curr_glob$date <- ym(curr_glob$date)
  
  if(region == "nwa" & enviro_layer == "bottom"){
            #filter for date range
              target_dates <- time(nwa_bo2_atm) >= ym("1995-01") & time(nwa_bo2_atm) <= ym("2019-12")
              nwa_bo2_sub <- nwa_bo2_atm[[target_dates]]
    
            #calculate median across area for Tpref and update crs for cropping
              med_bo2 <- median(nwa_bo2_sub)
              med_bo2_crs <- project(med_bo2, "EPSG:4326")
    
            #filter for species range
              nwa_bo2_crop <- crop(med_bo2_crs, hull, mask = TRUE)
    
            #take 10th percentile to get OxyThresh
              global_thresh <- terra::global(nwa_bo2_crop, quantile, probs = c(0.10), na.rm = TRUE)
              temp_dat$thresh_med = global_thresh[1,1]

  } else if(region == "nwa" & enviro_layer == "pelagic"){

    #filter for date range (do for temp and salinity to convert later to atm)
      target_dates <- time(nwa_o2_atm) >= ym("1995-01") & time(nwa_o2_atm) <= ym("2019-12")
      nwa_o2_sub <- nwa_o2_atm[[target_dates]]
    
    # get right depth layers
      min_layer <- which.min(abs(depth(nwa_o2_sub) - sp_dat$min_depth[i]))
      min_seq <- seq(from = min_layer, to = nlyr(nwa_o2_sub), by = length(unique(depth(nwa_o2_sub))))

      med_layer <- which.min(abs(depth(nwa_o2_sub) - sp_dat$med_depth[i]))
      med_seq <- seq(from = med_layer, to = nlyr(nwa_o2_sub), by = length(unique(depth(nwa_o2_sub))))
    
      quant_layer <- which.min(abs(depth(nwa_o2_sub) - sp_dat$quant_depth[i]))
      quant_seq <- seq(from = quant_layer, to = nlyr(nwa_o2_sub), by = length(unique(depth(nwa_o2_sub))))
    
      min_o2_rast <- nwa_o2_sub[[min_seq]]
      med_o2_rast <- nwa_o2_sub[[med_seq]]
      quant_o2_rast <- nwa_o2_sub[[quant_seq]]
  
    #calculate median across area for Tpref, Tmin, Tquant
    #min depth OxyThresh
      #crop and take median 
    min_o2_crop <- crop(min_o2_rast, hull, mask = TRUE)
    min_o2_med <- median(min_o2_crop)
    
      #get 10th percentile
    min_global_thresh <- terra::global(min_o2_med, quantile, probs = c(0.10), na.rm = TRUE)
    temp_dat$thresh_min = min_global_thresh[1,1]
    
    #median depth OxyThresh
      #crop and take median
    med_o2_crop <- crop(med_o2_rast, hull, mask = TRUE)
    med_o2_med <- median(med_o2_crop)

      #get 10th percentile
    med_global_thresh <- terra::global(med_o2_med, quantile, probs = c(0.10), na.rm = TRUE)
    temp_dat$thresh_med = med_global_thresh[1,1]

    #75% quantile depth OxyThresh
      #crop and take median
    quant_o2_crop <- crop(quant_o2_rast, hull, mask = TRUE)
    quant_o2_med <- median(quant_o2_crop)

     #get 10th percentile
    quant_global_thresh <- terra::global(quant_o2_med, quantile, probs = c(0.10), na.rm = TRUE)
    temp_dat$thresh_med = quant_global_thresh[1,1]
  
  } else if(region == "nep" & enviro_layer == "bottom"){
      


  } else if(region == "nep" & enviro_layer == "pelagic"){
      
   
  }
  } 

  #save values
  tpref_dat <- rbind(tpref_dat, temp_dat)
  }

return(tpref_dat)

}

