

# DESCRIPTIVE ANALYSES OF INVASIVE/EXOTIC SPECIES


# import data
source('scripts/2_taxonomic backbone.R')

# select expanding/invasive species
species_invasive <- read_excel("data/alien_flora_armenia.xlsx", sheet = "Sheet3")

# set colour palette
col = colorRampPalette(c("white","grey","black"))(3)


# most common families x status
temp1 <- species_invasive[species_invasive$family %in% names(which(table(species_invasive$family) > 3)),]
temp1$family <- factor(temp1$family, levels=c(names(table(temp1$family)[order(table(temp1$family), decreasing=T)])))
temp1$status <- factor(temp1$status, levels=c('casual','naturalized','invasive'))

ggplot(aes(x=family, fill=status), data=temp1) +
  geom_bar(position='dodge', color = "black") +
  scale_fill_manual(values = col) +
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



# Native range x status for invasive and expanding species using George's info
temp1 <- species_invasive
temp1$status <- factor(temp1$status, levels=c('casual','naturalized','invasive'))
temp1$origin <- factor(temp1$origin, levels=c(names(table(temp1$origin)[order(table(temp1$origin), decreasing=T)])))

ggplot(aes(x=origin, fill=status), data=temp1) +
  geom_bar(position='dodge', color = "black") +
  scale_fill_manual(values = col) +
  xlab('') + ylab('Number of species') +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black", size = 12, angle = -45, hjust = 0, vjust =0.5),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
         legend.title=element_blank(), legend.text = element_text(size=12), legend.position = c(0.8, 0.8))



# pathway x status
temp1 <- species_invasive[!is.na(species_invasive$pathway_CBD),]
temp1$pathway_CBD <- factor(temp1$pathway_CBD, levels=c(names(table(temp1$pathway_CBD)[order(table(temp1$pathway_CBD), decreasing=T)])))
temp1$status <- factor(temp1$status, levels=c('casual','naturalized','invasive'))

ggplot(aes(x=pathway_CBD, fill=status), data=temp1) +
  geom_bar(position='dodge', color = "black") +
  scale_fill_manual(values = col) +
  xlab('') + ylab('Number of species') +
  theme_classic() +
  theme (axis.text.x = element_text(color = "black", size = 12, angle = -45, hjust = 0, vjust =0.5),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
         legend.title=element_blank(), legend.text = element_text(size=12), legend.position = c(0.8, 0.8))


