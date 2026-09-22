#libraries
library(tidyverse)
library(here)
library(terra)
library(sf)

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

### nwa union hull #####
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

### nep union hull #####
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

### nwa management hulls #####
bathy_nwa <- rast(here("data/enviro/nwa/bathy/gebco_2026_n45.0_s25.0_w-82.0_e-66.0.nc"))
bathy_nwa <- ifel(bathy_nwa > 2, NA, bathy_nwa)
bathy_nwa <- ifel(bathy_nwa  <= -750, NA, bathy_nwa)
plot(bathy_nwa)

#full management domain
nwa_poly <- as.polygons(!is.na(bathy_nwa), dissolve = TRUE, aggregate = TRUE)
writeVector(nwa_poly, here("data/enviro/nwa/nwa_full.gpkg"), overwrite = TRUE)

#SE
se_ext <- ext(-82, -75, 25, 35)
se_poly <- crop(nwa_poly, se_ext)
writeVector(se_poly, here("data/enviro/nwa/nwa_SE.gpkg"), overwrite = TRUE)

#MAB/NE
mab_ext <- ext(-82, -66, 35, 45)
mab_poly <- crop(nwa_poly, mab_ext)
writeVector(mab_poly, here("data/enviro/nwa/nwa_NE_MAB.gpkg"), overwrite = TRUE)

#NE
ne_ext <- ext(-82, -62, 40.5, 45)
ne_poly <- crop(nwa_poly, ne_ext)
writeVector(ne_poly, here("data/enviro/nwa/nwa_NE.gpkg"), overwrite = TRUE)

### nep management hulls #####
bathy_nep <- rast(here("data/enviro/nep/bathy/gebco_2026_n55.0_s0.0_w-170.0_e-100.0.nc"))
domain <- ext(-127, -115, 30, 50)

bathy_nep <- crop(bathy_nep, domain)
bathy_nep <- ifel(bathy_nep > 2, NA, bathy_nep)
bathy_nep <- ifel(bathy_nep  <= -2300, NA, bathy_nep)
plot(bathy_nep)

nep_poly <- as.polygons(!is.na(bathy_nep), dissolve = TRUE)
writeVector(nep_poly, here("data/enviro/nep/nep_WC.gpkg"), overwrite = TRUE)
