

# COMPILATION OF ALL ALIEN TAXA PRESENT IN ARMENIA ACCORDING TO G. FAYVUSH, EITHER ORNAMENTAL OR NATURALIZED


library(tidyverse)
library(readxl)


# import all species found throughout the spreadsheets
species_status <- read_excel("data/alien_flora_armenia.xlsx", sheet = "allspecies")


# assign categories
species_status$status <- NA

# expanding/invasive species (Fayvush G & Tamanyan KG, 2014)
invasive_species <- read_excel("data/alien_flora_armenia.xlsx", sheet = "Sheet3") %>% dplyr::select(species) %>% deframe()
species_status$status[species_status$species %in% invasive_species] <- 'Invasive/expanding'

# watch species, not found yet in natural or semi-natural habitats but reproduce vegetatively or by self-seeding in places of introduction
# this is sheet 2 + GRIIS species
watch_species <- read_excel("data/alien_flora_armenia.xlsx", sheet = "Sheet2") %>% dplyr::select(species) %>% deframe()
watch_species <- watch_species[which(!(watch_species %in% invasive_species))]
species_status$status[species_status$species %in% watch_species] <- 'Watch list/potentially invasive'

griis_species <- read_excel("data/dwca-griis-armenia-v1.4/taxon.xlsx") %>%
  dplyr::filter(kingdom=='Plantae') %>% dplyr::select(species) %>% deframe()
griis_species <- griis_species[which(!(griis_species %in% c(invasive_species,watch_species)))]
species_status$status[species_status$species %in% griis_species] <- 'Watch list/potentially invasive'


# ornamental species present in botanical gardens and dendroparks, introduced before 1985
garden_species <- read_excel("data/alien_flora_armenia.xlsx", sheet = "Sheet1") %>% dplyr::select(species) %>% deframe()
garden_species <- garden_species[which(!(garden_species %in% c(invasive_species, watch_species, griis_species)))]
species_status$status[species_status$species %in% garden_species] <- 'Ornamental/only found in gardens'


# lets remove NAs (these correspond to species from risk assessment table)
table(species_status$status)
table(is.na(species_status$status))
species_status <- species_status[!(is.na(species_status$status)),]


