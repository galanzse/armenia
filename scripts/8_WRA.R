

# MAP ALIEN FLORA OF ARMENIA IN A PHYLOGENY


library(randomForest)
library(caTools)


source('scripts/2_taxonomic backbone.R')


# select expanding/invasive species
species_invasive <- read_excel("data/alien_flora_armenia.xlsx", sheet = "Sheet3") %>%
  left_join(read_excel("data/wra_armenia.xlsx", sheet="WRA_scores") %>%
              dplyr::select(species, score))


# score x earliest record
ggplot(aes(y=score, x=time_appearance2), data=species_invasive) +
  geom_point() +
  geom_smooth(method='loess', span=2, se=F) +
  xlab('Earliest record') + ylab('WRA score') +
  theme_classic()


# do George's categories match wra scores
species_invasive$status <- factor(species_invasive$status, levels=c('casual','naturalized','invasive'))
ggplot(aes(y=score, x=status, fill=status), data=species_invasive) +
  geom_boxplot() +
  ylab('WRA score') +
  theme_classic() +
  theme (axis.text.x = element_blank(), axis.title.x = element_blank(),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
         legend.title=element_blank(), legend.text = element_text(size=12), legend.position = 'top')


# wra
wra_armenia <- read_excel("data/wra_armenia.xlsx", sheet="WRA_questionnaire")
colnames(wra_armenia)[-1] <- substr(colnames(wra_armenia)[-1], 6, 50) # fix colnames
colnames(wra_armenia)[-1] <- gsub(" ", "_", colnames(wra_armenia)[-1])
wra_armenia$score <- NULL # do not need scores

# fix data
wra_armenia$`Weed_of_agriculture_/_horticulture_/_forestry`[wra_armenia$species=="Ambrosia artemisiifolia"] <- 'yes' # fill NAs
wra_armenia$`Self-fertilization`[wra_armenia$`Self-fertilization`=='no?'] <- 'no'
wra_armenia$Creates_a_fire_hazard_in_natural_ecosystems[wra_armenia$Creates_a_fire_hazard_in_natural_ecosystems=='no?'] <- 'no'
wra_armenia$Creates_a_fire_hazard_in_natural_ecosystems[wra_armenia$Creates_a_fire_hazard_in_natural_ecosystems=='no?'] <- 'no'
wra_armenia$`Broad_climate_suitability_(environmental_vers`[wra_armenia$`Broad_climate_suitability_(environmental_vers`==2] <- 'yes'

# remove variables with many NAs
colnames(wra_armenia)[colSums(is.na(wra_armenia))>0]
wra_armenia$Has_the_species_become_naturalized_where_grow <- NULL
wra_armenia$`Does_the_species_have_weedy_races?` <- NULL

# transform 2.02 (Quality of climate match data (0-low; 1-intermediate; 2-high)) into 0/1
table(wra_armenia$Quality_of_climate_match_data)
wra_armenia$Quality_of_climate_match_data <- 'yes'

# Minimum_generative_time does not make sense to remove it
wra_armenia$`Minimum_generative_time_(years)` <- NULL

# lets retain variables with variability
apply(wra_armenia, 2, table) %>% t()
wra_armenia$Quality_of_climate_match_data <- NULL
wra_armenia$Naturalized_beyond_native_range <- NULL
wra_armenia$Parasitic <- NULL       
wra_armenia$Host_for_recognized_pests_and_pathogens <- NULL
wra_armenia$Causes_allergies_or_is_otherwise_toxic_to_hum <- NULL
wra_armenia$Aquatic <- NULL
wra_armenia$Geophyte <- NULL 
wra_armenia$Evidence_of_substantial_reproductive_failure <- NULL
wra_armenia$Hybridizes_naturally <- NULL
wra_armenia$Requires_specialist_pollinators <- NULL
wra_armenia$Effective_natural_enemies_present_in_Armenia <- NULL

# species to rownames
wra_armenia <- as.data.frame(wra_armenia)
rownames(wra_armenia) <- wra_armenia$species; wra_armenia$species <- NULL
# turn into 1/0
wra_armenia[wra_armenia=='yes'] <- 1; wra_armenia[wra_armenia=='no'] <- 0


# # estimate redundancy
# df_chisq <- combn(colnames(wra_armenia), 2) %>% t() %>% as.data.frame()
# df_chisq$estimate <- NA
# df_chisq$p <- NA
# 
# for (i in 1:nrow(df_chisq)) { 
#   df_chisq$estimate[i] <- fisher.test(wra_armenia[,df_chisq$V1[i]], wra_armenia[,df_chisq$V2[i]])$estimate
#   df_chisq$p[i] <- fisher.test(wra_armenia[,df_chisq$V1[i]], wra_armenia[,df_chisq$V2[i]])$p.value
# }

# explore
df_chisq <- df_chisq %>% subset(estimate != 'Inf')
boxplot(df_chisq$estimate, main='odds ratio', ylim=c(-5,50))
abline(a=min(df_chisq$estimate[df_chisq$estimate>1 & df_chisq$p<0.05]), b=0, col='red', lty=2)
abline(a=max(df_chisq$estimate[df_chisq$estimate<1 & df_chisq$p<0.05]), b=0, col='red', lty=2)

# retain only
wra_armenia_filt <- wra_armenia %>%
  dplyr::select(df_chisq %>% subset(p < 0.05) %>% dplyr::select(V1,V2) %>% deframe() %>% unique())
wra_armenia_filt <- apply(wra_armenia_filt, 2, as.numeric)
wra_armenia_filt <- as.matrix(wra_armenia_filt)
rownames(wra_armenia_filt) <- rownames(wra_armenia)



# randomforest to predict origin
rf_wra <- wra_armenia_filt %>% as.data.frame() # prepare data
rf_wra$species <- rownames(rf_wra); rownames(rf_wra) <- NULL
rf_wra$species[rf_wra$species=='Picris echioides'] <- 'Helminthotheca echioides'
rf_wra <- as.data.frame(rf_wra)
rf_wra <- species_invasive[,c('species','status')] %>% left_join(rf_wra) %>% as.data.frame() %>% dplyr::select(-species)

rf1 <- randomForest(y=factor(rf_wra$status), x=rf_wra[,which(colnames(rf_wra)!='status')], ntree=500, mtry=5, replace=T, importance=T)
rf1
plot(rf1)
rf1$importance
varImpPlot(rf1, n.var=15, main='status ~')
# table(rf_wra$status, rf_wra$Well_controlled_by_herbicides)


