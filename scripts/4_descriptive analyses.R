

# DESCRIPTIVE ANALYSES OF INVASIVE/EXOTIC SPECIES


library(ggpubr)


# import data
source('scripts/2_taxonomic backbone.R')

# select expanding/invasive species
species_invasive <- read_excel("data/alien_flora_armenia.xlsx", sheet = "Sheet3")

# set colour palette
# col = colorRampPalette(c("white","grey","black"))(3)


# most common families x status
temp1 <- species_invasive[species_invasive$family %in% names(which(table(species_invasive$family) > 3)),]
temp1$family <- factor(temp1$family, levels=c(names(table(temp1$family)[order(table(temp1$family), decreasing=T)])))
temp1$status <- factor(temp1$status, levels=c('casual','naturalized','invasive'))

ggplot(aes(x=family, fill=status), data=temp1) +
  geom_bar(position='dodge', color = "black") +
  # scale_fill_manual(values = col) +
  xlab('') + ylab('Number of species') +
  theme_classic() +
  theme (axis.text.x = element_text(color = "black", size = 12, angle = -45, hjust = .5, vjust =0.2),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
         legend.title=element_blank(), legend.text = element_text(size=12), legend.position = c(0.8, 0.8))


# life form x status
temp1 <- species_invasive
temp1$life_form <- factor(temp1$life_form, levels=c(names(table(temp1$life_form)[order(table(temp1$life_form), decreasing=T)])))
temp1$status <- factor(temp1$status, levels=c('casual','naturalized','invasive'))

ggplot(aes(x=life_form, fill=status), data=temp1) +
  geom_bar(position='dodge', color = "black") +
  scale_fill_manual(values = col) +
  xlab('') + ylab('Number of species') +
  theme_classic() +
  theme (axis.text.x = element_text(color = "black", size = 12, angle = -45, hjust = .5, vjust =0.2),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
         legend.title=element_blank(), legend.text = element_text(size=12), legend.position = c(0.8, 0.8))


# growth form x status
temp1 <- species_invasive
temp1$growth_form <- factor(temp1$growth_form, levels=c(names(table(temp1$growth_form)[order(table(temp1$growth_form), decreasing=T)])))
temp1$status <- factor(temp1$status, levels=c('casual','naturalized','invasive'))

ggplot(aes(x=growth_form, fill=status), data=temp1) +
  geom_bar(position='dodge', color = "black") +
  scale_fill_manual(values = col) +
  xlab('') + ylab('Number of species') +
  theme_classic() +
  theme (axis.text.x = element_text(color = "black", size = 12, angle = -45, hjust = .5, vjust =0.2),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
         legend.title=element_blank(), legend.text = element_text(size=12), legend.position = c(0.8, 0.8))


# longevity x status
temp1 <- species_invasive
temp1$life_cycle <- factor(temp1$life_cycle, levels=c(names(table(temp1$life_cycle)[order(table(temp1$life_cycle), decreasing=T)])))
temp1$status <- factor(temp1$status, levels=c('casual','naturalized','invasive'))

ggplot(aes(x=life_cycle, fill=status), data=temp1) +
  geom_bar(position='dodge', color = "black") +
  scale_fill_manual(values = col) +
  xlab('') + ylab('Number of species') +
  theme_classic() +
  theme (axis.text.x = element_text(color = "black", size = 12, angle = -45, hjust = .5, vjust =0.2),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
         legend.title=element_blank(), legend.text = element_text(size=12), legend.position = c(0.8, 0.8))


# disturbance x status
temp1 <- species_invasive
temp1$disturbance <- factor(temp1$disturbance, levels=c(names(table(temp1$disturbance)[order(table(temp1$disturbance), decreasing=T)])))
temp1$status <- factor(temp1$status, levels=c('casual','naturalized','invasive'))

ggplot(aes(x=disturbance, fill=status), data=temp1) +
  geom_bar(position='dodge', color = "black") +
  scale_fill_manual(values = col) +
  xlab('') + ylab('Number of species') +
  theme_classic() +
  theme (axis.text.x = element_text(color = "black", size = 12, angle = -45, hjust = .5, vjust =0.2),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
         legend.title=element_blank(), legend.text = element_text(size=12), legend.position = c(0.8, 0.8))



# intentionality x status
temp1 <- species_invasive[!is.na(species_invasive$pathway),]
temp1$pathway <- factor(temp1$pathway, levels=c(names(table(temp1$pathway)[order(table(temp1$pathway), decreasing=T)])))
temp1$status <- factor(temp1$status, levels=c('casual','naturalized','invasive'))

ggplot(aes(x=pathway, fill=status), data=temp1) +
  geom_bar(position='dodge', color = "black") +
  scale_fill_manual(values = col) +
  xlab('') + ylab('Number of species') +
  theme_classic() +
  theme (axis.text.x = element_text(color = "black", size = 12, angle = -45, hjust = 0, vjust =0.5),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
         legend.title=element_blank(), legend.text = element_text(size=12), legend.position = c(0.8, 0.8))



# pathway x status
pathway_CBD <- read_excel("data/alien_flora_armenia.xlsx", sheet="pathway_CBD")
pathway_CBD[is.na(pathway_CBD)] <- 0
colnames(pathway_CBD) <- gsub(" ", "_", colnames(pathway_CBD))
pathway_CBD <- left_join(species_invasive[,c('species','status')], pathway_CBD)
pathway_CBD$status <- factor(pathway_CBD$status, levels=c('casual','naturalized','invasive'))

pathway_CBD_long <- pathway_CBD %>%
  pivot_longer(3:ncol(pathway_CBD), names_to='pathway', values_to='yn') %>%
  subset(yn==1)

table(pathway_CBD_long$status, pathway_CBD_long$pathway) %>% t()

pathway_CBD_long$pathway <- factor(pathway_CBD_long$pathway,
                                   levels=c(names(table(pathway_CBD_long$pathway)[order(table(pathway_CBD_long$pathway), decreasing=T)])))

ggplot(aes(x=pathway, fill=status),
       data=pathway_CBD_long[pathway_CBD_long$pathway %in% names(table(pathway_CBD_long$pathway)[1:5]),]) +
  geom_bar(position='dodge', color = "black") +
  scale_fill_manual(values = col) +
  xlab('') + ylab('Number of species') +
  theme_classic() +
  theme (axis.text.x = element_text(color = "black", size = 12, angle = -45, hjust = 0, vjust =0.5),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
         legend.title=element_blank(), legend.text = element_text(size=12), legend.position = c(0.8, 0.8))



# Native range x status for invasive and expanding species using George's info
temp1 <- species_invasive
temp1$status <- factor(temp1$status, levels=c('casual','naturalized','invasive'))
temp1 <- temp1 %>% subset(!(origin %in% c('Caucasus','Himalayas','Tropics')))
temp1$origin <- temp1$origin %>% factor(levels=c(names(table(temp1$origin)[order(table(temp1$origin), decreasing=T)])))

g1 <- ggplot(aes(x=origin, fill=status), data=temp1) +
  geom_bar(position='dodge', color = "black") +
  # scale_fill_manual(values = col) +
  xlab('') + ylab('Number of species') +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black", size = 12, angle = -45, hjust = 0, vjust =0.5),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
        legend.title=element_blank(), legend.text = element_text(size=12), legend.position = 'top',
        plot.margin = margin(t=30, r = 40))


# ipbes x status
IPBES_cat <- read_excel("data/alien_flora_armenia.xlsx", sheet="IPBES")
IPBES_cat[is.na(IPBES_cat)] <- 0
# colnames(IPBES_cat) <- gsub(" ", "_", colnames(IPBES_cat))
IPBES_cat <- left_join(species_invasive[,c('species','status')], IPBES_cat)

IPBES_cat_long <- IPBES_cat %>%
  pivot_longer(3:ncol(IPBES_cat), names_to='IPBES', values_to='yn') %>%
  subset(yn==1)

IPBES_cat$status <- factor(IPBES_cat$status, levels=c('casual','naturalized','invasive'))

table(IPBES_cat_long$status, IPBES_cat_long$IPBES) %>% t()

IPBES_cat_long$IPBES <- factor(IPBES_cat_long$IPBES,
                               levels=c(names(table(IPBES_cat_long$IPBES)[order(table(IPBES_cat_long$IPBES), decreasing=T)])))

mylables <- c('Urban/Semi-urban', 'Inland surface waters and\nwater bodies/freshwater', 'Temperate and boreal\nforests and woodlands',
              'Deserts and\nxeric shrublands', 'Wetlands', 'Temperate\ngrasslands')

g2 <- ggplot(aes(x=IPBES, fill=status), data=IPBES_cat_long) +
  geom_bar(position='dodge', color = "black") +
  # scale_fill_manual(values = col) +
  xlab('') + ylab('Number of species') +
  scale_x_discrete(breaks=unique(IPBES_cat_long$IPBES), labels=mylables) +
  theme_classic() +
  theme (axis.text.x = element_text(color = "black", size = 12, angle = -45, hjust = 0, vjust =0.5),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
         legend.title=element_blank(), legend.text = element_text(size=12), legend.position = 'none',
         plot.margin = margin(t=30, r=40))


ggarrange(g1, g2, labels=c('a','b'), nrow=2)


