

# DOWLOAD OCCURRENCES FROM GBIF AND RESEARCH GRADE OBSERVATIONS FROM INATURALIST

library(rgbif)
library(rinat)
library(rnaturalearth)
library(terra)

# load species list
source('scripts/2_taxonomic backbone.R')


# GBIF ####

# download occurrences and convert into dataframe
lt_speciesxgbif <- occ_search(taxonKey=species_status$species_gbif, country='AM', hasCoordinate=T)
df_speciesxgbif <- lt_speciesxgbif %>% lapply(`[[`, 'data') %>% bind_rows()

# filter
df_speciesxgbif$scientificName %>% unique()
df_speciesxgbif$basisOfRecord %>% table()


# INATURALIST ####

# save data
lt_speciesxinat <- list()

# rinat does not allow for multiple queries, so loop
for (i in 1:length(speciesxgbif$species)) {
  
  # access data and remove errors
  try(lt_speciesxinat[[i]] <- get_inat_obs(taxon_name=speciesxgbif$species[i], geo=T,
                                       bounds=c(38.73, 43.12, 41.42, 46.50),
                                       maxresults=10000, meta=F) %>%
    filter(quality_grade=='research'),
    silent=T)
  
  # progress
  print(paste(round(i/length(speciesxgbif$species),2)*100, '%'))
}

df_speciesxrinat <- do.call("rbind", lt_speciesxinat)


# MERGE OBSERVATIONS ####

df2_speciesxgbif <- df_speciesxgbif %>% dplyr::select(species, decimalLatitude, decimalLongitude)
df2_speciesxrinat <- df_speciesxrinat %>% dplyr::select(scientific_name, latitude, longitude)
colnames(df2_speciesxgbif) <- c('species','y','x')
colnames(df2_speciesxrinat) <- c('species','y','x')

mgd_occurrences <- rbind(df2_speciesxgbif, df2_speciesxrinat)
# write.csv(mgd_occurrences, 'results/mgd_occurrences.csv')

# map
armenia_map <- ne_countries(country='Armenia', scale=10)[1] %>% vect() %>% as.polygons()

# points
pts_mgd_occurrences <- vect(mgd_occurrences, geom=c("x", "y"))
# pts_gbif <- vect(df2_speciesxgbif, geom=c("x", "y"))
# pts_inat <- vect(df2_speciesxrinat, geom=c("x", "y"))


plot(armenia_map)
points(pts_mgd_occurrences, col='black')
# points(pts_gbif, col='red')
# points(pts_inat, col='blue')


