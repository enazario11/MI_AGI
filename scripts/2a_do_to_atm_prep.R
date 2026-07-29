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