
#libraries
library(tidyverse)
library(here)
library(lubridate)

#load dataset
dat_glob <- readRDS(here("data/fishglob/fishglob_usa.rds")) %>% 
  filter(survey == "NEUS" | survey == "SEUS" | survey == "WCANN" | survey == "WCTRI") %>%
  filter(year >= 1993 & num > 0) %>% 
  filter(accepted_name != "Scomber japonicus" | #pacific mackeral somehow had locs from NEUS/SEUS surveys?
        (accepted_name == "Scomber japonicus" & survey %in% c("WCANN", "WCTRI")))

dat_spp <- read.csv(here("data/fishglob/spp_metdat.csv"))

# MI/AGI species list -----------------------------------------------------
for(i in 1:nrow(dat_spp)){
  #filter df for given species
  curr_sp <- paste(dat_spp$Genus[i], dat_spp$Species[i])
  temp_glob <- dat_glob %>% filter(accepted_name == curr_sp)

  #extract metadata for given species
  dat_spp$rows[i] <- nrow(temp_glob)
  dat_spp$min_year[i] <- min(temp_glob$year, na.rm = TRUE)
  dat_spp$max_year[i] <- max(temp_glob$year, na.rm = TRUE)
  dat_spp$n_year[i] <- sum(length(unique(temp_glob$year)))
  dat_spp$survey_1[i] <- unique(temp_glob$survey_unit)[1]
  dat_spp$survey_2[i] <- unique(temp_glob$survey_unit)[2]
  dat_spp$survey_3[i] <- unique(temp_glob$survey_unit)[3]
  dat_spp$survey_4[i] <- unique(temp_glob$survey_unit)[4]
  dat_spp$survey_5[i] <- unique(temp_glob$survey_unit)[5]

}

write.csv(dat_spp, here("data/fishglob/glob_metdat.csv"), row.names = FALSE)

# look at depth layers for pelagic/schooling fish
sp_dat <- read.csv(here("data/fishglob/glob_metdat.csv"))
pel_dat <- sp_dat %>% filter(enviro_layer == "pelagic")

for(i in 1:nrow(pel_dat)){

  curr_sp <- unique(pel_dat$Common.name)[i]
  temp_dat = sp_dat %>% filter(Common.name == curr_sp)

  curr_glob <- dat_glob %>% 
    filter(accepted_name == paste(temp_dat$Genus, temp_dat$Species) & num > 0) 

  #filter for survey rows where more than the 25% quantile were pulled to make sure it's "true" habitat
  low_quant_num <- quantile(curr_glob$num, 0.25)
  curr_glob <- curr_glob %>%
    filter(num > low_quant_num)

  pel_dat$med_depth[i] <- median(curr_glob$depth, na.rm = TRUE)
  pel_dat$min_depth[i] <- min(curr_glob$depth, na.rm = TRUE)
  pel_dat$top_quant_depth[i] <- quantile(curr_glob$num, 0.75)

}

