#### libraries #####
library(tidyverse)
library(here)
library(terra)
library(sf)
source(here("functions/oxy_demand_functions.R"))

### load NWA data #####
#### bottom temperature #####
nwa_btemp <- rast(here("data/enviro/nwa/temp/raw/tob.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

#### all z temperature #####
nwa_temp <- rast(here("data/enviro/nwa/temp/raw/thetao.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

#### bottom salinity #####
nwa_bsal <- rast(here("data/enviro/nwa/salinity/raw/sob.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

#### all z salinity #####
nwa_sal <- rast(here("data/enviro/nwa/salinity/raw/so.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

#### bottom DO #####
#nwa_bo2 <- rast(here("data/enviro/nwa/do/raw/btm_o2.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

#convert to atm
#nwa_bo2_atm <- do_to_atm(do = nwa_bo2, t = nwa_btemp, s = nwa_bsal, thresh = FALSE)
#writeCDF(nwa_bo2_atm, here("data/enviro/nwa/do/nwa_bo2_atm.nc"))

nwa_bo2_atm <- rast(here("data/enviro/nwa/do/nwa_bo2_atm.nc"))

#### all z do #####
nwa_o2 <- rast(here("data/enviro/nwa/do/raw/o2.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

#convert to atm 
nwa_o2_atm <- do_to_atm(do = nwa_o2, t = nwa_temp, s = nwa_sal, thresh = FALSE)
writeCDF(nwa_o2_atm, here("data/enviro/nwa/do/nwa_o2_atm.nc"))

### load NEP data #####
#### bottom temperature #####
nep_btemp <- rast(here("data/enviro/nep/temp/raw/tob.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))

#### all z temperature #####
nep_temp <- rast(here("data/enviro/nep/temp/raw/thetao.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))

#### bottom salinity #####
nep_bsal <- rast(here("data/enviro/nep/salinity/raw/sob.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))

#### all z salinity #####
nep_sal <- rast(here("data/enviro/nep/salinity/raw/so.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))

#### bottom DO #####
# nep_bo2 <- rast(here("data/enviro/nep/do/raw/btm_o2.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))

# #convert to atm
# nep_bo2_atm <- do_to_atm(do = nep_bo2, t = nep_btemp, s = nep_bsal, thresh = FALSE)
# writeCDF(nep_bo2_atm, here("data/enviro/nep/do/nep_bo2_atm.nc"))

nep_bo2_atm <- rast(here("data/enviro/nep/do/nep_bo2_atm.nc"))

#### all z do #####
nep_o2 <- rast(here("data/enviro/nep/do/raw/o2.nep.full.hcast.monthly.regrid.r20250912.199301-202506.nc"))

#convert to atm 
nep_o2_atm <- do_to_atm(do = nep_o2, t = nep_temp, s = nep_sal, thresh = FALSE)
writeCDF(nep_o2_atm, here("data/enviro/nep/do/nep_o2_atm.nc"))

### Tpref ######
sp_dat <- read.csv(here("data/fishglob/glob_metdat.csv"))

load(here("data/fishglob/FishGlob_public_clean.RData"))
dat_glob <- data
dat_glob <- dat_glob %>% #readRDS(here("data/fishglob/fishglob_usa.rds"))
  filter(country == "United States") %>%
  filter(survey == "NEUS" | survey == "SEUS" | survey == "WCANN" | survey == "WCTRI") %>%
  filter(year >= 1993 & num > 0) %>% 
  filter(accepted_name != "Scomber japonicus" | #pacific mackeral somehow had locs from NEUS/SEUS surveys?
        (accepted_name == "Scomber japonicus" & survey %in% c("WCANN", "WCTRI")))

get_Tpref <- function(sp_dat, temp_dat, region){
  
  sp_dat$Tpref_med <- NA
  sp_dat$Tpref_min <- NA
  sp_dat$Tpref_quant <- NA
  tpref_dat <- data.frame()

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
    
            #calculate median across area for Tpref and update crs for cropping
              med_btemp <- median(nwa_btemp_sub)
              med_btemp_crs <- project(med_btemp, "EPSG:4326")
    
            #filter for species range
               nwa_btemp_crop <- crop(med_btemp_crs, hull, mask = TRUE)
    
            #take mean
              global_avg <- terra::global(nwa_btemp_crop, fun = "mean", na.rm = TRUE)
            
              temp_dat$Tpref_med = global_avg[1,1]

  } else if(region == "nwa" & enviro_layer == "pelagic"){

    #filter for date range
      target_dates <- time(nwa_temp) >= ym("1995-01") & time(nwa_temp) <= ym("2019-12")
      nwa_temp_sub <- nwa_temp[[target_dates]]
    
    # get right depth layers
      min_layer <- which.min(abs(depth(nwa_temp_sub) - sp_dat$min_depth[i]))
      min_seq <- seq(from = min_layer, to = nlyr(nwa_temp_sub), by = length(unique(depth(nwa_temp_sub))))

      med_layer <- which.min(abs(depth(nwa_temp_sub) - sp_dat$med_depth[i]))
      med_seq <- seq(from = med_layer, to = nlyr(nwa_temp_sub), by = length(unique(depth(nwa_temp_sub))))
    
      quant_layer <- which.min(abs(depth(nwa_temp_sub) - sp_dat$quant_depth[i]))
      quant_seq <- seq(from = quant_layer, to = nlyr(nwa_temp_sub), by = length(unique(depth(nwa_temp_sub))))
    
      min_temp_rast <- nwa_temp_sub[[min_seq]]
      med_temp_rast <- nwa_temp_sub[[med_seq]]
      quant_temp_rast <- nwa_temp_sub[[quant_seq]]

    #calculate median across area for Tpref, Tmin, Tquant
    #min depth Tpref
    min_temp_med <- median(min_temp_rast)

    #Update CRS, crop, and take mean
    min_temp_crs <- project(min_temp_med, "EPSG:4326")
    min_temp_crop <- crop(min_temp_crs, hull, mask = TRUE)
    min_global_avg <- terra::global(min_temp_crop, fun = "mean", na.rm = TRUE)
  
    temp_dat$Tpref_min = min_global_avg[1,1]
    
    #median depth Tpref
    med_temp_med <- median(med_temp_rast)

    #Update CRS, crop, and take mean
    med_temp_crs <- project(med_temp_med, "EPSG:4326")
    med_temp_crop <- crop(med_temp_crs, hull, mask = TRUE)
    med_global_avg <- terra::global(med_temp_crop, fun = "mean", na.rm = TRUE)
  
    temp_dat$Tpref_med = med_global_avg[1,1]

    #75% quantile depth Tpref
    quant_temp_med <- median(quant_temp_rast)

    #Update CRS, crop, and take mean
    quant_temp_crs <- project(quant_temp_med, "EPSG:4326")
    quant_temp_crop <- crop(quant_temp_crs, hull, mask = TRUE)
    quant_global_avg <- terra::global(quant_temp_crop, fun = "mean", na.rm = TRUE)
  
    temp_dat$Tpref_quant = quant_global_avg[1,1]
    
  } else if(region == "nep" & enviro_layer == "bottom"){
      
    #filter for date range
      target_dates <- time(nep_btemp) >= ym("1995-01") & time(nep_btemp) <= ym("2019-12")
      nep_btemp_sub <- nep_btemp[[target_dates]]
    
    #calculate median across area for Tpref and update crs for cropping
      med_btemp <- median(nep_btemp_sub)
      med_btemp_crs <- project(med_btemp, "EPSG:4326")
      med_btemp_rot <- rotate(med_btemp_crs)

    #filter for species range
      nep_btemp_crop <- crop(med_btemp_rot, hull, mask = TRUE)

    #take mean
      global_avg <- terra::global(nep_btemp_crop, fun = "mean", na.rm = TRUE)
    
      temp_dat$Tpref_med = global_avg[1,1]

  } else if(region == "nep" & enviro_layer == "pelagic"){
      
    #filter for date range
      target_dates <- time(nep_temp) >= ym("1995-01") & time(nep_temp) <= ym("2019-12")
      nep_temp_sub <- nep_temp[[target_dates]]
    
     # get right depth layers
      min_layer <- which.min(abs(depth(nep_temp_sub) - sp_dat$min_depth[i]))
      min_seq <- seq(from = min_layer, to = nlyr(nep_temp_sub), by = length(unique(depth(nep_temp_sub))))

      med_layer <- which.min(abs(depth(nep_temp_sub) - sp_dat$med_depth[i]))
      med_seq <- seq(from = med_layer, to = nlyr(nep_temp_sub), by = length(unique(depth(nep_temp_sub))))
    
      quant_layer <- which.min(abs(depth(nep_temp_sub) - sp_dat$quant_depth[i]))
      quant_seq <- seq(from = quant_layer, to = nlyr(nep_temp_sub), by = length(unique(depth(nep_temp_sub))))
    
      min_temp_rast <- nep_temp_sub[[min_seq]]
      med_temp_rast <- nep_temp_sub[[med_seq]]
      quant_temp_rast <- nep_temp_sub[[quant_seq]]

    #calculate median across area for Tpref, Tmin, Tquant
    #min depth Tpref
    min_temp_med <- median(min_temp_rast)

    #Update CRS, crop, and take mean
    min_temp_crs <- project(min_temp_med, "EPSG:4326")
    min_temp_rot <- rotate(min_temp_crs)
    min_temp_crop <- crop(min_temp_rot, hull, mask = TRUE)
    min_global_avg <- terra::global(min_temp_crop, fun = "mean", na.rm = TRUE)
  
    temp_dat$Tpref_min = min_global_avg[1,1]
    
    #median depth Tpref
    med_temp_med <- median(med_temp_rast)

    #Update CRS, crop, and take mean
    med_temp_crs <- project(med_temp_med, "EPSG:4326")
    med_temp_rot <- rotate(med_temp_crs)
    med_temp_crop <- crop(med_temp_rot, hull, mask = TRUE)
    med_global_avg <- terra::global(med_temp_crop, fun = "mean", na.rm = TRUE)
  
    temp_dat$Tpref_med = med_global_avg[1,1]

    #75% quantile depth Tpref
    quant_temp_med <- median(quant_temp_rast)

    #Update CRS, crop, and take mean
    quant_temp_crs <- project(quant_temp_med, "EPSG:4326")
    quant_temp_rot <- rotate(quant_temp_crs)
    quant_temp_crop <- crop(quant_temp_rot, hull, mask = TRUE)
    quant_global_avg <- terra::global(quant_temp_crop, fun = "mean", na.rm = TRUE)
  
    temp_dat$Tpref_quant = quant_global_avg[1,1]

  }
  } 

  #save values
  tpref_dat <- rbind(tpref_dat, temp_dat)
  }

return(tpref_dat)

}

#simplify input data
sp_dat_nwa <- sp_dat %>% filter(region == "nwa")
nwa_tpref <- get_Tpref(sp_dat = sp_dat_nwa, region = "nwa")
saveRDS(nwa_tpref, here("data/agi/nwa_tpref.rds"))

sp_dat_nep <- sp_dat %>% filter(region == "nep")
nep_tpref <- get_Tpref(sp_dat = sp_dat_nep, region = "nep")
saveRDS(nep_tpref, here("data/agi/nep_tpref.rds"))

all_tpref <- rbind(nwa_tpref, nep_tpref) %>% select(c(Common.name, Tpref_med, Tpref_min, Tpref_quant))
all_tpref2 <- merge(sp_dat, all_tpref, all.x = TRUE)

#save tpref data
saveRDS(all_tpref2, file = here("data/agi/sp_dat_tpref.rds"))
sp_dat_tpref <- readRDS(here("data/agi/sp_dat_tpref.rds"))

### OxyThresh #####
#crop o2, temp, and sal rasters according to one large unionized hull (project first). See how long converting this to atm takes otherwise start species-specific step there. If quick, take species hull crop and calc median then quantile.
#create separate polygons for nep and nwa
sp_pts <- st_as_sf(
  dat_glob,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

sp_hulls <- sp_pts %>%
  group_by(accepted_name) %>%
  summarize(geometry = st_combine(geometry), .groups = "drop") %>%
  st_convex_hull()

hull_union <- st_union(sp_hulls)



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
      target_dates <- time(nwa_o2) >= ym("1995-01") & time(nwa_o2) <= ym("2019-12")
      nwa_o2_sub <- nwa_o2[[target_dates]]
      nwa_temp_sub <- nwa_temp[[target_dates]]
      nwa_sal_sub <- nwa_sal[[target_dates]]
    
    # get right depth layers
      min_layer <- which.min(abs(depth(nwa_o2_sub) - sp_dat$min_depth[i]))
      min_seq <- seq(from = min_layer, to = nlyr(nwa_o2_sub), by = length(unique(depth(nwa_o2_sub))))

      med_layer <- which.min(abs(depth(nwa_o2_sub) - sp_dat$med_depth[i]))
      med_seq <- seq(from = med_layer, to = nlyr(nwa_o2_sub), by = length(unique(depth(nwa_o2_sub))))
    
      quant_layer <- which.min(abs(depth(nwa_o2_sub) - sp_dat$quant_depth[i]))
      quant_seq <- seq(from = quant_layer, to = nlyr(nwa_o2_sub), by = length(unique(depth(nwa_o2_sub))))
    
      min_o2_rast <- nwa_o2_sub[[min_seq]]
      min_temp_rast <- nwa_temp_sub[[min_seq]]
      min_sal_rast <- nwa_sal_sub[[min_seq]]

      med_o2_rast <- nwa_o2_sub[[med_seq]]
      med_temp_rast <- nwa_temp_sub[[med_seq]]
      med_sal_rast <- nwa_sal_sub[[med_seq]]

      quant_o2_rast <- nwa_o2_sub[[quant_seq]]
      quant_temp_rast <- nwa_temp_sub[[quant_seq]]
      quant_sal_rast <- nwa_sal_sub[[quant_seq]]

    #calculate median across area for Tpref, Tmin, Tquant
    #min depth OxyThresh
      #crop
    min_o2_crop <- crop(min_o2_rast, hull, mask = TRUE)
    min_temp_crop <- crop(min_temp_rast, hull, mask = TRUE)
    min_sal_crop <- crop(min_sal_rast, hull, mask = TRUE)

      #convert do to atm
    min_o2_atm <- do_to_atm(do = min_o2_crop, t = min_temp_crop, s = min_sal_crop)

      #take median across layer
    min_o2_med <- median(min_o2_atm)

      #project
    min_o2_crs <- project(min_o2_med, "EPSG:4326")
    
      #get 10th percentile
    min_global_thresh <- terra::global(min_o2_crs, quantile, probs = c(0.10), na.rm = TRUE)
    temp_dat$thresh_min = min_global_thresh[1,1]
    
    #median depth OxyThresh
    med_temp_med <- median(med_temp_rast)

    #Update CRS, crop, and take mean
    med_temp_crs <- project(med_temp_med, "EPSG:4326")
    med_temp_crop <- crop(med_temp_crs, hull, mask = TRUE)
    med_global_avg <- terra::global(med_temp_crop, fun = "mean", na.rm = TRUE)
  
    temp_dat$Tpref_med = med_global_avg[1,1]

    #75% quantile depth OxyThresh
    quant_temp_med <- median(quant_temp_rast)

    #Update CRS, crop, and take mean
    quant_temp_crs <- project(quant_temp_med, "EPSG:4326")
    quant_temp_crop <- crop(quant_temp_crs, hull, mask = TRUE)
    quant_global_avg <- terra::global(quant_temp_crop, fun = "mean", na.rm = TRUE)
  
    temp_dat$Tpref_quant = quant_global_avg[1,1]
    
  } else if(region == "nep" & enviro_layer == "bottom"){
      
    #filter for date range
      target_dates <- time(nep_btemp) >= ym("1995-01") & time(nep_btemp) <= ym("2019-12")
      nep_btemp_sub <- nep_btemp[[target_dates]]
    
    #calculate median across area for Tpref and update crs for cropping
      med_btemp <- median(nep_btemp_sub)
      med_btemp_crs <- project(med_btemp, "EPSG:4326")
      med_btemp_rot <- rotate(med_btemp_crs)

    #filter for species range
      nep_btemp_crop <- crop(med_btemp_rot, hull, mask = TRUE)

    #take mean
      global_avg <- terra::global(nep_btemp_crop, fun = "mean", na.rm = TRUE)
    
      temp_dat$Tpref_med = global_avg[1,1]

  } else if(region == "nep" & enviro_layer == "pelagic"){
      
    #filter for date range
      target_dates <- time(nep_temp) >= ym("1995-01") & time(nep_temp) <= ym("2019-12")
      nep_temp_sub <- nep_temp[[target_dates]]
    
     # get right depth layers
      min_layer <- which.min(abs(depth(nep_temp_sub) - sp_dat$min_depth[i]))
      min_seq <- seq(from = min_layer, to = nlyr(nep_temp_sub), by = length(unique(depth(nep_temp_sub))))

      med_layer <- which.min(abs(depth(nep_temp_sub) - sp_dat$med_depth[i]))
      med_seq <- seq(from = med_layer, to = nlyr(nep_temp_sub), by = length(unique(depth(nep_temp_sub))))
    
      quant_layer <- which.min(abs(depth(nep_temp_sub) - sp_dat$quant_depth[i]))
      quant_seq <- seq(from = quant_layer, to = nlyr(nep_temp_sub), by = length(unique(depth(nep_temp_sub))))
    
      min_temp_rast <- nep_temp_sub[[min_seq]]
      med_temp_rast <- nep_temp_sub[[med_seq]]
      quant_temp_rast <- nep_temp_sub[[quant_seq]]

    #calculate median across area for Tpref, Tmin, Tquant
    #min depth Tpref
    min_temp_med <- median(min_temp_rast)

    #Update CRS, crop, and take mean
    min_temp_crs <- project(min_temp_med, "EPSG:4326")
    min_temp_rot <- rotate(min_temp_crs)
    min_temp_crop <- crop(min_temp_rot, hull, mask = TRUE)
    min_global_avg <- terra::global(min_temp_crop, fun = "mean", na.rm = TRUE)
  
    temp_dat$Tpref_min = min_global_avg[1,1]
    
    #median depth Tpref
    med_temp_med <- median(med_temp_rast)

    #Update CRS, crop, and take mean
    med_temp_crs <- project(med_temp_med, "EPSG:4326")
    med_temp_rot <- rotate(med_temp_crs)
    med_temp_crop <- crop(med_temp_rot, hull, mask = TRUE)
    med_global_avg <- terra::global(med_temp_crop, fun = "mean", na.rm = TRUE)
  
    temp_dat$Tpref_med = med_global_avg[1,1]

    #75% quantile depth Tpref
    quant_temp_med <- median(quant_temp_rast)

    #Update CRS, crop, and take mean
    quant_temp_crs <- project(quant_temp_med, "EPSG:4326")
    quant_temp_rot <- rotate(quant_temp_crs)
    quant_temp_crop <- crop(quant_temp_rot, hull, mask = TRUE)
    quant_global_avg <- terra::global(quant_temp_crop, fun = "mean", na.rm = TRUE)
  
    temp_dat$Tpref_quant = quant_global_avg[1,1]

  }
  } 

  #save values
  tpref_dat <- rbind(tpref_dat, temp_dat)
  }

return(tpref_dat)

}


