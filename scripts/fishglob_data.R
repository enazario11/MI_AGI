
#libraries
library(tidyverse)
library(here)
library(lubridate)

#load dataset
dat_glob <- readRDS(here("data/fishglob/fishglob_usa.rds")) %>% 
  filter(survey == "NEUS" | survey == "SEUS" | survey == "WCANN" | survey == "WCTRI") %>%
  filter(year >= 1993)
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

write.csv(dat_spp, here("data/temp/glob_metdat.csv"))

# look at depth layers for pelagic/schooling fish
temp_glob <- dat_glob %>% filter(accepted_name == "Clupea harengus")
plot(temp_glob$depth, temp_glob$num) #50m

temp_glob <- dat_glob %>% filter(accepted_name == "Scomber scombrus")
plot(temp_glob$depth, temp_glob$num) #50m

temp_glob <- dat_glob %>% filter(accepted_name == "Pomatomus saltatrix")
plot(temp_glob$depth, temp_glob$num) #25m

temp_glob <- dat_glob %>% filter(accepted_name == "Peprilus triacanthus")
plot(temp_glob$depth, temp_glob$num) #100m

temp_glob <- dat_glob %>% filter(accepted_name == "Scomberomorus cavalla")
plot(temp_glob$depth, temp_glob$num) #25m

temp_glob <- dat_glob %>% filter(accepted_name == "Engraulis mordax")
plot(temp_glob$depth, temp_glob$num) #25m

temp_glob <- dat_glob %>% filter(accepted_name == "Clupea pallasii")
plot(temp_glob$depth, temp_glob$num) #100m

temp_glob <- dat_glob %>% filter(accepted_name == "Scomber japonicus")
plot(temp_glob$depth, temp_glob$num) #100m

temp_glob <- dat_glob %>% filter(accepted_name == "Sardinops sagax")
plot(temp_glob$depth, temp_glob$num) #100m

temp_glob <- dat_glob %>% filter(accepted_name == "Epinephelus morio")
plot(temp_glob$depth, temp_glob$num) #50m

temp_glob <- dat_glob %>% filter(accepted_name == "Urophycis chuss")
plot(temp_glob$depth, temp_glob$num) #100m

temp_glob <- dat_glob %>% filter(accepted_name == "Stenotomus chrysops")
plot(temp_glob$depth, temp_glob$num) #25m

temp_glob <- dat_glob %>% filter(accepted_name == "Merluccius bilinearis")
plot(temp_glob$depth, temp_glob$num) #1000m

temp_glob <- dat_glob %>% filter(accepted_name == "Scomberomorus maculatus")
plot(temp_glob$depth, temp_glob$num) #50m
