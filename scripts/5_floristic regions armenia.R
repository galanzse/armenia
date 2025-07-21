

# EXPLORE PATTERNS ACCORDING TO FLORISTIC REGIONS ###


library(terra)
library(rnaturalearth)
library(vegan)
library(ggalluvial)


# import data
source('scripts/2_taxonomic backbone.R')

# select expanding/invasive species
species_invasive <- read_excel("data/alien_flora_armenia.xlsx", sheet = "Sheet3")
species_invasive$status <- factor(species_invasive$status, levels=c('casual','naturalized','invasive'))

# import floristic regions
floristic_reg <- read_excel("data/alien_flora_armenia.xlsx",  sheet = " floristic regions")
colnames(floristic_reg)[-1] <- c("Upper Akhuryan", "Shirak", "Lori", "Idjevan", "Aparan", "Sevan", "Areguni", "Yerevan", "Darelegis", "N.Zangezur", "S.Zangezur", "Meghri")
floristic_reg_long <- floristic_reg %>% pivot_longer(!species, names_to = "flor_region", values_to = "count") %>% na.omit()



# NUMBER OF REGIONS INVADED PER SPECIES AND DIFFERENCES AMONG STATUS
# create new variable: number of floristic regions where the species is present
species_invasive$n_flor_reg <- NA
for (i in 1:nrow(species_invasive)) {
  species_invasive$n_flor_reg[i] <- floristic_reg_long %>% subset(species==species_invasive$species[i]) %>% nrow()
}

# most frequent species
species_invasive$species[order(species_invasive$n_flor_reg, decreasing=T)]

# plot per group
ggplot(aes(x=status, y=n_flor_reg), data=species_invasive) +
  geom_boxplot(position='dodge', color = "black") +
  xlab('') + ylab('Number of floristic regions occupied') +
  theme_classic() +
  theme (axis.text.x = element_text(color = "black", size = 12, angle = -45, hjust = 0, vjust =0.5),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
         legend.title=element_blank(), legend.text = element_text(size=12), legend.position = c(0.8, 0.8))



# FLORISTIC SIMILARITIES AMONG REGIONS
floristic_mat <- floristic_reg
colnames(floristic_mat)[-1] <- c("Upper Akhuryan", "Shirak", "Lori", "Idjevan", "Aparan", "Sevan", "Areguni", "Yerevan", "Darelegis", "N.Zangezur", "S.Zangezur", "Meghri") # fix names

floristic_mat[is.na(floristic_mat)] <- 0 # fill NAs
floristic_mat <- as.data.frame(floristic_mat)
rownames(floristic_mat) <- floristic_mat$species # create matrix
floristic_mat$species <- NULL
floristic_mat <- as.matrix(floristic_mat)

flreg_NMDS <- metaMDS(t(floristic_mat), k=2)
stressplot(flreg_NMDS)

ordiplot(flreg_NMDS,type="n")
orditorp(flreg_NMDS,display="species",col="red",air=0.01)
orditorp(flreg_NMDS,display="sites",cex=1.25,air=0.01)



# EVALUATE THE RICHEST FLORISTIC REGIONS IN TERMS OF INVASIVE FLORA
colSums(floristic_mat)[order(colSums(floristic_mat), decreasing=T)]



# ASSOCIATIONS BETWEEN REGIONS OF ORIGIN AND RECIPIENT FLORISTIC REGION
# head(vaccinations) # template

armenia_alluvial <- expand.grid(unique(species_invasive$origin), unique(colnames(floristic_reg)[-1]))
colnames(armenia_alluvial) <- c('origin','destiny')
armenia_alluvial$freq <- NA

for (i in 1:nrow(armenia_alluvial)) { # loop to count species in common
  armenia_alluvial$freq[i] <- intersect(floristic_reg$species[!is.na(deframe(floristic_reg[,armenia_alluvial[i,2]]))],
                                        species_invasive$species[species_invasive$origin==armenia_alluvial[i,1]]) %>% length()
}

# remove least frequent origin categories
armenia_alluvial <- armenia_alluvial %>% subset(!(origin %in% c('Caucasus', 'Himalayas', 'Tropics')))

ggplot(data = armenia_alluvial, aes(axis1=origin, axis2=destiny, y=freq)) +
  geom_alluvium(aes(fill=destiny)) +
  geom_stratum() +
  geom_text(stat="stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Native region", "Recipient floristic region")) +
  theme_void() +
  theme(legend.position='n')



# LIFE FORMS AND RECIPIENT FLORISTIC REGIONS
temp1 <- expand.grid(colnames(floristic_reg)[-1], unique(species_invasive$life_form))
colnames(temp1) <- c('region','lifeform')
temp1$freq <- NA
for (i in 1:nrow(temp1)) {
  temp1$freq[i] <- intersect(floristic_reg$species[!is.na(deframe(floristic_reg[,temp1[i,1]]))],
                             species_invasive$species[species_invasive$life_form==temp1[i,2]]) %>% length()
}

temp1 <- temp1[temp1$lifeform!='helophyte',]
temp1$lifeform <- factor(temp1$lifeform, levels=c('helophyte','therophyte','geophyte','hemicryptophyte','chamaephyte','phanerophyte'))


ggplot(aes(x=region, y=freq, fill=lifeform), data=temp1) +
  geom_col(position='dodge') +
  # scale_fill_manual(values = col) +
  xlab('Floristic region') + ylab('Number of species') +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black", size = 12, angle = -45, hjust = 0, vjust =0.5),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
        legend.title=element_blank(), legend.text = element_text(size=12), legend.position = c(0.8, 0.8))


