
#libraries
library(tidyverse)
library(here)

#load dataset
dat_glob <- readRDS(here("data/fishglob/FishGlob_public_metadata_clean.RDS"))
dat_glob_nam <- dat_glob %>% filter(continent == 'n_america')

#striped sea bass 
dat_glob_bass <- dat_glob_nam %>% filter(accepted_name == 'Morone saxatilis')
table(dat_glob_bass$survey) #only included for Northeast US NOAA surveys, 1 Gulf St. Lawrence South

# atlantic cod
dat_glob_cod <- dat_glob_nam %>% filter(accepted_name == 'Gadus morhua')
table(dat_glob_cod$survey) #included for Northeast US NOAA surveys, Gulf St. Lawrence, and Canada Maritimes


# MI/AGI species list -----------------------------------------------------

met_sp <- data.frame(sp_names = gsub("\t", " ", c("Anguilla	japonica",
               "Carassius	carassius",
               "Centropristis	striata",
               "Cyclopterus	lumpus",
               "Cyprinus	carpio",
               "Dentex	dentex",
               "Etheostoma	flabellare",
               "Gadus	morhua",
               "Gadus	macrocephalus",
               "Lates	calcarifer",
               "Morone	saxatilis",
               "Oncorhynchus	mykiss",
               "Oreochromis	niloticus",
               "Ostorhinchus	doederleini",
               "Parabramis	pekinensis",
               "Paralichthys	dentatus",
               "Pomacentrus	moluccensis",
               "Salmo	salar",
               "Sciaenops	ocellatus",
               "Scyliorhinus	canicula",
               "Seriola	lalandi",
               "Squalas	acanthias",
               "Stenobrachius	leucopsarus",
               "Tautogolabrus	adspersus",
               "Chrysoblephus	laticeps",
               "Rachycentron	canadum")))

met_sp$fish_glob = "NA"
for(i in 1:nrow(met_sp)){
  sp_temp = met_sp$sp_names[i]
  
  fish_glob = sum(str_detect(dat_glob$accepted_name, sp_temp)) > 0
  
  met_sp$fish_glob[i] <- fish_glob
}

#met_glob_filter <- met_sp %>% filter(fish_glob == "FALSE")


# filter fishglob ---------------------------------------------------------
met_glob_sp <- met_sp %>% filter(fish_glob == "TRUE")

dat_glob_met <- filter(dat_glob, str_detect(accepted_name, paste(met_glob_sp$sp_names, collapse = "|")))

#check if match
SameElements <- function(a, b) return(identical(sort(a), sort(b)))

SameElements(unique(dat_glob_met$accepted_name), unique(met_glob_sp$sp_names)) #TRUE


# explore fishglob --------------------------------------------------------

survey_n <- dat_glob_met %>% group_by(accepted_name, survey) %>% summarise(n = n()) #tally by species and survey unit

dat_glob_met %>% group_by(accepted_name) %>% summarise(n = n()) %>% filter(n > 5) #tally by species, removed Dentex dentex (n = 2) and Oncorhynchus mykiss (n = 1)



