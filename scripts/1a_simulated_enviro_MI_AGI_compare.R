# packages
library(tidyverse)
library(tidyquant)
library(terra)
library(sf)
library(here)
library(rnaturalearth)
library(rnaturalearthdata)
source(here("functions/oxy_demand_functions.R"))
set.seed(0904)


## simulate DO (atm) and temp (C) data ####
enviro_sim <- data.frame(lat = runif(100, min = 34, max = 42), 
                         long = runif(100, min = -77, max = -68), 
                         DO_atm = runif(100, min = 0.10, max = 0.20), 
                         temp = runif(100, min = 12, max = 30))
DO_thresh <- quantile(enviro_sim$DO_atm)[1]


## calculate MI and AGI for black sea bass ####
### MI ####
enviro_sim <- enviro_sim %>% mutate(DO_kpa = DO_atm*101.325)
enviro_sim$mi <- MI_calc(A0 = 0.00040728, Bn = 1, DO_kPa = enviro_sim$DO_kpa, E0 = 0.27, kB = 0.00008617333262, T_C = enviro_sim$temp)
hist(enviro_sim$mi)

### AGI ####
enviro_sim$agi_o2_demand <- OxyDemand(Tpref = 22, PO2_thresh = DO_thresh, T_C = enviro_sim$temp, W = 498.95, d = 0.700, K = 0.231, j2 = 8, j1 = 4.5, 
                                      Linf = 34.1, LwA = 0.02661, LwB = 2.8)
enviro_sim$agi <- enviro_sim$DO_atm/enviro_sim$agi_o2_demand
hist(enviro_sim$agi)

## plot MI and AGI ####
### MI ####
#DO vs. MI
ggplot(enviro_sim, aes(DO_kpa, mi))+
  geom_point(size = 2)+
  theme_tq()

#temp vs. MI
ggplot(enviro_sim, aes(temp, mi))+
  geom_point(size = 2)+
  theme_tq()

#temp, DO, MI
ggplot(enviro_sim, aes(temp, DO_kpa))+
  geom_point(size = 4, aes(color = mi))+
  theme_tq()

### AGI ####
#DO vs. AGI
ggplot(enviro_sim, aes(DO_atm, agi))+
  geom_point(size = 2)+
  theme_tq()

#temp vs. AGI
ggplot(enviro_sim, aes(temp, agi))+
  geom_point(size = 2)+
  theme_tq()

#temp, DO, AGI
ggplot(enviro_sim, aes(temp, DO_atm))+
  geom_point(size = 4, aes(color = agi))+
  theme_tq()

#coastline
shore <- ne_download(scale = "medium", type = "land", category = "physical", returnclass = "sf")
mid_atlantic_bbox <- st_bbox(c(xmin = -77, xmax = -68, ymin = 34, ymax = 42), crs = st_crs(shore))
mid_atlantic_coast <- st_crop(shore, mid_atlantic_bbox)

#MI
ggplot() +
  geom_point(data = enviro_sim, aes(x = long, y = lat, color = mi), size = 3) +
  scale_color_viridis_c(option = "plasma", name = "AGI") +
  geom_sf(data = mid_atlantic_coast, fill = "grey70") +
  coord_sf(xlim = c(-77, -68), ylim = c(34.5, 42), expand = FALSE) +
  theme_tq()

#AGI
ggplot() +
  geom_point(data = enviro_sim, aes(x = long, y = lat, color = agi), size = 3) +
  scale_color_viridis_c(option = "plasma", name = "AGI") +
  geom_sf(data = mid_atlantic_coast, fill = "grey70") +
  coord_sf(xlim = c(-77, -68), ylim = c(34.5, 42), expand = FALSE) +
  theme_tq()



