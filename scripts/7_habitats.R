

# EXPLORE HABITATS AND IPBES CATEGORIES


library(vegan)


# import data
source('scripts/2_taxonomic backbone.R')


# select expanding/invasive species
species_invasive <- read_excel("data/alien_flora_armenia.xlsx", sheet = "Sheet3")


# ipbes
ipbes <- read_excel("data/alien_flora_armenia.xlsx", sheet="IPBES") %>% as.data.frame()
ipbes[is.na(ipbes)] <- 0 # fill NAs

ipbes_mat <- ipbes
rownames(ipbes_mat) <- ipbes_mat$species # create matrix
ipbes_mat$species <- NULL
ipbes_mat <- as.matrix(ipbes_mat)


# most invaded
ipbes_long <- ipbes %>% pivot_longer(!species, names_to="ipbes", values_to="count") %>% subset(count==1)
ipbes_long <- ipbes_long[ipbes_long$ipbes %in% names(which(table(ipbes_long$ipbes) > 4)),]
ipbes_long$ipbes <- factor(ipbes_long$ipbes,
                                levels=c(names(table(ipbes_long$ipbes)[order(table(ipbes_long$ipbes), decreasing=T)])))

ipbes_long <- ipbes_long %>% dplyr::select(-count) %>% left_join(species_invasive[,c('species','status','life_form')])
ipbes_long$status <- factor(ipbes_long$status, levels=c('casual','naturalized','invasive'))

# set colour palette
col = colorRampPalette(c("white","grey","black"))(3)

ggplot(aes(x=ipbes, fill=status), data=ipbes_long) + # stringr::str_wrap(habitat, 30)
  geom_bar(position='dodge', color = "black") +
  scale_fill_manual(values = col) +
  xlab('') + ylab('Number of species') +
  theme_classic() +
  theme (axis.text.x = element_text(color = "black", size = 12, angle = -45, hjust = .15, vjust =0),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
         legend.title=element_blank(), legend.text = element_text(size=12), legend.position = c(0.8, 0.8))

ggplot(aes(x=ipbes, fill=life_form), data=ipbes_long) + # stringr::str_wrap(habitat, 30)
  geom_bar(position='dodge', color = "black") +
  # scale_fill_manual(values = col) +
  xlab('') + ylab('Number of species') +
  theme_classic() +
  theme (axis.text.x = element_text(color = "black", size = 12, angle = -45, hjust = .15, vjust =0),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
         legend.title=element_blank(), legend.text = element_text(size=12), legend.position = c(0.8, 0.6))



# how similar are species according to the habitats they invade?
flreg_NMDS <- metaMDS(t(ipbes_mat), k=2)
stressplot(flreg_NMDS)
ordiplot(flreg_NMDS,type="n")
orditorp(flreg_NMDS,display="species",col="red",air=0.01)
orditorp(flreg_NMDS,display="sites",cex=1.25,air=0.01)


