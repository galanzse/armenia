

# EXPLORE TRAITS FROM TRY AND SEE WHETHER WE CAN CONSIDER THIS FOR FUTURE ANALYSES


library(data.table)
# install.packages("writexl")
# library(writexl)


source('scripts/2_taxonomic backbone.R')



# import data
# try_armenia <- fread("data/try data/42782.txt")

remove long string variables
# try_armenia$Reference <- NULL
# try_armenia$Comment <- NULL

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



# import invasive and watch species
invasive_species <- read_excel("data/alien_flora_armenia.xlsx", sheet="Sheet3") %>%
  dplyr::select(species, family, life_form, life_cycle, growth_form, status) 
watch_species <- read_excel("data/alien_flora_armenia.xlsx", sheet="Sheet2") %>%
  dplyr::select(species, family, life_form, life_cycle, growth_form) %>%
  subset(!(species %in% invasive_species$species))
watch_species$status <- 'watch'

# merge species and traits
traits_final <- rbind(invasive_species, watch_species) %>% left_join(try_means_wide)
table(traits_final$status)

# long format
traits_final_long <- traits_final %>% pivot_longer(7:26, names_to='trait', values_to='value') %>%
  subset(trait!='frost_tolerance2') %>%
  na.omit()
traits_final_long$status <- factor(traits_final_long$status, levels=c('watch','casual','naturalized','invasive'))

traits_final_long$trait <- factor(traits_final_long$trait,
                                  levels=c("height_vegetative","height_generative","longevity","frost_tolerance1","drought_tolerance",
                                           "seed_mass", "propagule_mass","seed_length", "seed_shedding","leaf_length",
                                           "onset_flowering","length_flowering","end_flowering","leaf_width","leaf_aspect",
                                           "SLA","LDMC","Nmass","leaf_form"))

# LHS x status: only invasive species
ggplot(aes(fill=status, y=log(value)), data=traits_final_long) +
  geom_boxplot() +
  facet_wrap(.~trait, scales='free', nrow=4) +
  theme_classic() +
  theme(legend.position='top', legend.title=element_blank(),
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())


