#libraries
library(tidyverse)
library(here)
library(terra)
library(sf)
library(rnaturalearth)
library(tidyterra)
source(here("functions/oxy_demand_functions.R"))

#load tpref and oxythresh data
sp_dat_tpref <- readRDS(here("data/agi/sp_dat_tpref.rds"))
sp_dat_oxythresh <- readRDS(here("data/agi/sp_dat_oxythresh.rds"))

  #combine to one df
oxytemp <- sp_dat_oxythresh %>% select(c(Common.name, thresh_med, thresh_min, thresh_quant))
tpref_oxythresh <- merge(sp_dat_tpref, oxytemp, by = "Common.name", all = TRUE)
agi_temp <- tpref_oxythresh %>% select(c(Common.name, Tpref_min, Tpref_med, Tpref_quant, thresh_min, thresh_med, thresh_quant))

#load fishbase coef data
agi_coef <- read.csv(here("data/agi/agi_coef.csv"))

  #combine fishbase coefs and tpref/oxythresh values
agi_dat <- agi_coef %>% left_join(agi_temp, by = "Common.name")

  #exploratory analyses of tpref and oxythresh
agi_viz <- agi_dat[!duplicated(agi_dat$Common.name), ]
agi_tpref <- agi_viz %>% 
  pivot_longer(
    cols = c(Tpref_min, Tpref_med, Tpref_quant), 
    names_to = "Tpref", 
    values_to = "value"
  )

agi_tpref %>%
  mutate(Tpref = fct_relevel(Tpref, "Tpref_min", "Tpref_med", "Tpref_quant")) %>%
  ggplot(aes(Tpref, value)) + 
    geom_point(aes(color = Tpref, shape = region), size = 3) + 
    facet_wrap(~Common.name) +
    theme_bw()+
    scale_color_manual(values = c("#01448A", "#0B69A6", "#5F9ABA"))


agi_oxythresh <- agi_viz %>% 
  pivot_longer(
    cols = c(thresh_min, thresh_med, thresh_quant), 
    names_to = "oxythresh", 
    values_to = "value"
  )

agi_oxythresh %>%
  mutate(oxythresh = fct_relevel(oxythresh, "thresh_min", "thresh_med", "thresh_quant")) %>%
  ggplot(aes(oxythresh, value)) + 
    geom_point(aes(color = oxythresh, shape = region), size = 3) + 
    facet_wrap(~Common.name) +
    theme_bw()+
    scale_color_manual(values = c("#01448A", "#0B69A6", "#5F9ABA"))

#load species hulls
sp_hull <- list.files("data/fish_hull", full.names = TRUE)

#calculate map of AGI 
AGI <- function(sp_name, sex, season, enviro){ #consider adding enviro dat paths as arguments
  
  #isolate to species of interest
  agi_dat2 <- agi_dat %>% filter(Common.name == sp_name)

  if(agi_dat2$region == "nwa" && enviro == "bottom"){
    #load enviro data
    nwa_bo2 <- rast(here("data/enviro/nwa/do/atm/nwa_bo2_atm.nc"))
    nwa_bto <- rast(here("data/enviro/nwa/temp/raw/tob.nwa.full.hcast.monthly.regrid.r20250715.199301-202312.nc"))

    #get species hull
    sp_hull_path <- sp_hull[grepl(sp_name, sp_hull)]
    sp_hull_file <- st_read(paste0(sp_hull_path, "/", agi_dat2$Genus, " ", agi_dat2$Species, ".shp"))

    #crop enviro files to species hull
    nwa_bo2_crop <- crop(nwa_bo2, sp_hull_file, mask = TRUE)
    nwa_bto_crop <- crop(nwa_bto, sp_hull_file, mask = TRUE)

    #calculate agi -- place holder weight of 1 kg
    oxy_demand <- OxyDemand(Tpref = agi_dat2$Tpref_med, PO2_thresh = agi_dat2$thresh_med, T_C = nwa_bto_crop, 
                            W = 1000, K = agi_dat2$K, Linf = agi_dat2$Linf, LwA = agi_dat2$LwA, LwB = agi_dat2$LwB)
    
    agi <- nwa_bo2_crop/oxy_demand

    #take median across time
    agi_med <- median(agi, na.rm = TRUE)
  }
}

#nwa species
  #load land mask
land <- ne_countries(scale = "large", returnclass = "sf")

  #get agi raster
agi_test <- AGI(sp_name = "American plaice", enviro = "bottom")

ggplot() +
  geom_spatraster(data = agi_test) +
  geom_sf(data = land, fill = "grey85", colour = "grey30", linewidth = 0.2) +
  coord_sf(xlim = as.vector(ext(agi_test))[1:2] + c(-2, 2),
           ylim = as.vector(ext(agi_test))[3:4] + c(-2, 2),
           expand = FALSE) +
  scale_fill_viridis_c(na.value = "transparent", name = "AGI") +
  labs(x = NULL, y = NULL) +
  theme_bw()
