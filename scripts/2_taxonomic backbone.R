

# GET A STANDARD TAXONOMY FOR POSTERIOR ANALYSES: GBIF


# library(rgbif)

# load species list
source('scripts/1_import species.R')


# GBIF
# species_gbif <- name_backbone_checklist(name=species_status$species)
# write.table(species_gbif, 'results/species_gbif.txt')
species_gbif <- read.csv("results/species_gbif.txt", sep="")
table(species_gbif$status)

# add column
species_status$species_gbif <- species_gbif$species
# fill empty names with original names
species_status$species_gbif[is.na(species_gbif$species)] <- species_status$species[is.na(species_gbif$species)]

# genera
species_status$genus <- stringr::str_extract(species_status$species_gbif, '\\w*') # I am using GBIF because it yields less errors than original taxonomy

# lets fix some genera and  family names
species_status$genus[species_status$genus=='Lophiolepis'] <- 'Cirsium' # fix genera: https://powo.science.kew.org/results?q=Lophiolepis
species_status$genus[species_status$genus=='Cyclachaena'] <- 'Euphrosyne'
species_status$genus[species_status$genus=='Mahonia'] <- 'Berberis'
species_status$genus[species_status$genus=='Cerasus'] <- 'Prunus'
species_status$genus[species_status$genus=='Pyrethrum'] <- 'Tanacetum'

species_status$family[species_status$species=='Mimulus hybridus'] <- 'Phrymaceae' # fix families
species_status$family[species_status$family=='Cercidophyllaceae'] <- 'Cercidiphyllaceae'


# merge
colnames(species_gbif)[colnames(species_gbif)=='status'] <- 'synonym'
colnames(species_gbif)[colnames(species_gbif)=='species'] <- 'species_gbif'
species_status <- merge(species_status[,c('species','status','species_gbif')],
                      unique(species_gbif[,c('family','genus','species_gbif')]),
                      by='species_gbif')



rm(species_gbif)
