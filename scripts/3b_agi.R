#libraries
library(tidyverse)
library(here)
library(terra)
library(sf)
library(rnaturalearth)
library(tidyterra)
library(patchwork)
source(here("functions/agi_calc.R"))

#load tpref and oxythresh data
sp_dat_tpref <- readRDS(here("data/agi/sp_dat_tpref.rds"))
sp_dat_oxythresh <- readRDS(here("data/agi/sp_dat_oxythresh.rds"))

  #combine to one df
oxytemp <- sp_dat_oxythresh %>% select(c(Common.name, thresh_med, thresh_min, thresh_quant))
tpref_oxythresh <- merge(sp_dat_tpref, oxytemp, by = "Common.name", all = TRUE)
agi_temp <- tpref_oxythresh %>% select(c(Common.name, min_depth, med_depth, quant_depth, Tpref_min, Tpref_med, Tpref_quant, thresh_min, thresh_med, thresh_quant))

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

nep_temp <- agi_tpref %>%
  filter(region == "nep") %>%
  mutate(Tpref = fct_relevel(Tpref, "Tpref_min", "Tpref_med", "Tpref_quant")) %>%
  ggplot(aes(Tpref, value)) + 
    geom_point(aes(color = Tpref), size = 4) + 
    facet_wrap(~Common.name) +
    theme_bw()+
    scale_color_manual(values = c("#5F9ABA", "#0B69A6", "#01448A")) + 
    xlab("") + 
    ggtitle("nep")

nwa_temp <- agi_tpref %>%
  filter(region == "nwa") %>%
  mutate(Tpref = fct_relevel(Tpref, "Tpref_min", "Tpref_med", "Tpref_quant")) %>%
  ggplot(aes(Tpref, value)) + 
    geom_point(aes(color = Tpref), shape = 17, size = 4) + 
    facet_wrap(~Common.name) +
    theme_bw()+
    scale_color_manual(values = c("#5F9ABA", "#0B69A6","#01448A")) + 
    xlab("") + 
    ggtitle("nwa")

nep_temp / nwa_temp

agi_oxythresh <- agi_viz %>% 
  pivot_longer(
    cols = c(thresh_min, thresh_med, thresh_quant), 
    names_to = "oxythresh", 
    values_to = "value"
  )

nep_ox <- agi_oxythresh %>%
  filter(region == "nep") %>%
  mutate(oxythresh = fct_relevel(oxythresh, "thresh_min", "thresh_med", "thresh_quant")) %>%
  ggplot(aes(oxythresh, value)) + 
    geom_point(aes(color = oxythresh), size = 4) + 
    facet_wrap(~Common.name) +
    theme_bw()+
    scale_color_manual(values = c("#01448A", "#0B69A6", "#5F9ABA"))+
    xlab("")+
    ggtitle("nep")

nwa_ox <- agi_oxythresh %>%
  filter(region == "nwa") %>%
  mutate(oxythresh = fct_relevel(oxythresh, "thresh_min", "thresh_med", "thresh_quant")) %>%
  ggplot(aes(oxythresh, value)) + 
    geom_point(aes(color = oxythresh), shape = 17, size = 4) + 
    facet_wrap(~Common.name) +
    theme_bw()+
    scale_color_manual(values = c("#01448A", "#0B69A6", "#5F9ABA"))+
    xlab("")+
    ggtitle("nwa")

nep_ox/nwa_ox

#plot median agi values across survey period -- not specifying agi values for sex, season, or region
  #load land mask
land <- ne_countries(scale = "large", returnclass = "sf")

  #agi raster bottom species
agi_bot <- AGI(sp_name = "Black sea bass", enviro = "bottom")
agi_crit_bot <- get_crit(agi = agi_bot, enviro = "bottom")

ggplot() +
  geom_spatraster(data = agi_bot) +
  geom_spatvector(data = agi_crit_bot, color = "black", fill = NA, linewidth = 1) +
  geom_sf(data = land, fill = "grey85", colour = "grey30", linewidth = 0.2) +
  coord_sf(xlim = as.vector(ext(agi_bot))[1:2] + c(-2, 2),
           ylim = as.vector(ext(agi_bot))[3:4] + c(-2, 2),
           expand = FALSE) +
  scale_fill_whitebox_c(palette = "muted", direction = -1) +
  facet_wrap(~lyr, labeller = label_wrap_gen(width = 45)) +
  labs(x = NULL, y = NULL, fill = "AGI") +
  tidyquant::theme_tq() + 
  theme(strip.text = element_text(size = 16), 
        legend.position = "right")
  
  #agi raster pelagic species
agi_pel <- AGI(sp_name = "Northern anchovy", enviro = "pelagic")
agi_crit_pel <- get_crit(agi_pel, enviro = "pelagic")

ggplot() +
  geom_spatraster(data = agi_pel) +
  geom_spatvector(data = agi_crit_pel, color = "black", fill = NA, linewidth = 1) +
  geom_sf(data = land, fill = "grey85", colour = "grey30", linewidth = 0.2) +
  coord_sf(xlim = as.vector(ext(agi_pel))[1:2] + c(-2, 2),
           ylim = as.vector(ext(agi_pel))[3:4] + c(-2, 2),
           expand = FALSE) +
  scale_fill_whitebox_c(palette = "muted", direction = -1) +
  labs(x = NULL, y = NULL, fill = "AGI") +
  tidyquant::theme_tq() + 
  facet_wrap(~factor(lyr, levels = names(agi_pel)), labeller = label_wrap_gen(width = 25)) +
  theme(strip.text = element_text(size = 14), 
        legend.position = "right")
  