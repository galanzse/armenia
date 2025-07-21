

# EXPLORE TRAITS FROM TRY AND SEE WHETHER WE CAN CONSIDER THIS FOR FUTURE ANALYSES


source('scripts/2_taxonomic backbone.R')

library(data.table)
# install.packages("writexl")
# library(writexl)


# import data
# try_armenia <- fread("data/try data/42782.txt")

# remove long string variables
try_armenia$Reference <- NULL
try_armenia$Comment <- NULL

# # remove covariates
# try_armenia <- try_armenia[!(is.na(try_armenia$TraitID)),]
# 
# # convert into df, select important variables and remove duplicates
# try_armenia <- try_armenia %>% as.data.frame() %>%
#   dplyr::select(AccSpeciesName, TraitName, DataName, OriglName, OrigValueStr, OrigUnitStr) %>%
#   unique()

# the table is quite complicated, lets have a look at it on excel
# write.csv(try_armenia, "data/try data/try_armenia_filt.csv")

# summarize curated dataset
try_means <- read_excel("data/try data/try_armenia_filt.xlsx", sheet = "continuous")%>%
  group_by(AccSpeciesName, TraitName) %>%
  summarise(mean_trait = median(OrigValueStr))
colnames(try_means)[colnames(try_means)=="AccSpeciesName"] <- 'species'

# long to wide
try_means_wide <- try_means %>% pivot_wider(names_from = TraitName, values_from = mean_trait)

# check NAs
vect_NA <- colSums(!(is.na(try_means_wide)))/nrow(try_means_wide)*100
vect_NA[order(vect_NA, decreasing=T)]
# for the three key traits (plant height, seed mass and SLA) there is over 71% coverage



# LHS x status: only invasive species
statusxtraits <- left_join(species_invasive, try_means_wide, by='species')
colnames(try_means_wide)
ggplot(aes(x=status, y=log(height_vegetative)), data=statusxtraits) +
  geom_boxplot()
ggplot(aes(x=status, y=log(SLA)), data=statusxtraits) +
  geom_boxplot()
ggplot(aes(x=status, y=log(seed_mass)), data=statusxtraits) +
  geom_boxplot()



# LHS x expanding/watch
# import invasive and watch species, merge species and traits
invasive_species <- read_excel("data/alien_flora_armenia.xlsx", sheet="Sheet3") %>%
  dplyr::select(species, family, life_form, life_cycle, growth_form) %>%
  left_join(try_means_wide)
invasive_species$status <- 'invasive'

watch_species <- read_excel("data/alien_flora_armenia.xlsx", sheet="Sheet2") %>%
  dplyr::select(species, family, life_form, life_cycle, growth_form) %>%
  subset(!(species %in% invasive_species$species)) %>%
  left_join(try_means_wide)
watch_species$status <- 'watch'

all_species <- rbind(invasive_species, watch_species)
ggplot(aes(x=status, y=log(height_vegetative)), data=all_species) +
  geom_boxplot()
ggplot(aes(x=status, y=log(SLA)), data=all_species) +
  geom_boxplot()
ggplot(aes(x=status, y=log(seed_mass)), data=all_species) +
  geom_boxplot()


