

library(tidyverse)
library(readxl)
library(red)
library(ggpubr)


# import data
species_invasive <- read_excel("data/alien_flora_armenia.xlsx", sheet = "Sheet3")
species_invasive$status <- factor(species_invasive$status, levels=c('casual','naturalized','invasive'))

# create new variable: number of floristic regions where the species is present
floristic_reg <- read_excel("data/alien_flora_armenia.xlsx",  sheet = "floristic regions")
colnames(floristic_reg)[-1] <- c("Upper Akhuryan", "Shirak", "Lori", "Idjevan", "Aparan", "Sevan", "Areguni", "Yerevan", "Darelegis", "N.Zangezur", "S.Zangezur", "Meghri")
floristic_reg_long <- floristic_reg %>% pivot_longer(!species, names_to = "flor_region", values_to = "count") %>% na.omit()
species_invasive$n_flor_reg <- NA
for (i in 1:nrow(species_invasive)) {
  species_invasive$n_flor_reg[i] <- floristic_reg_long %>% subset(species==species_invasive$species[i]) %>% nrow()
}



# species accumulation curve
df_cumulative <- species_invasive %>% dplyr::select(time_appearance2) %>%
  dplyr::arrange(time_appearance2) %>%  dplyr::mutate(cumulative_count = row_number())

ggplot(df_cumulative, aes(x = time_appearance2, y = cumulative_count)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_classic() +
  ylab('Species accumulation') + xlab(NULL) +
  theme (axis.text.x = element_text(color = "black", size = 12, angle = -45, hjust = .5, vjust =0.2),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5))

# species accumulation x status
df_cumulative <- species_invasive %>% dplyr::select(status, time_appearance2) %>%
  group_by(status) %>%
  dplyr::arrange(time_appearance2) %>%  dplyr::mutate(cumulative_count = row_number())
df_cumulative$status <- factor(df_cumulative$status, levels=c('casual','naturalized','invasive'))

g1 <- ggplot(df_cumulative, aes(x = time_appearance2, y = cumulative_count, color=status)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_classic() +
  geom_vline(xintercept=c(1700,1800,1900,2000), linetype=3) +
  ylab('Species accumulation') + xlab(NULL) +
  theme (axis.text.x = element_blank(),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
         legend.position='top', legend.title=element_blank(), legend.text = element_text(size=14))
         # plot.margin = margin(l = 55))



# residence x status
temp1 <- species_invasive[!is.na(species_invasive$pathway),]
temp1$pathway <- factor(temp1$pathway, levels=c('Intentional','Unintentional','Self-settlement'))
temp1$status <- factor(temp1$status, levels=c('casual','naturalized','invasive'))

ggplot(aes(x=time_appearance2, y=pathway), data=temp1) +
  geom_boxplot(position='dodge', color = "black") +
  scale_fill_manual(values = col) +
  ylab(NULL) + xlab(NULL) +
  theme_classic() +
  geom_vline(xintercept=c(1700,1800,1900,2000), linetype=3) +
  theme (axis.text.x = element_blank(),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
         legend.title=element_blank(), legend.text = element_text(size=12), legend.position = c(0.8, 0.8),
         plot.margin = margin(r = 0))


# number floristic regions x residence time
g3 <- ggplot(aes(x=time_appearance2, y=n_flor_reg, colour=status, fill=status), data=species_invasive) +
  # scale_color_manual(values=col) +
  geom_point(size = 2) +
  ylab('Number of floristic regions occupied') + xlab('Earliest record') +
  geom_smooth(method='loess', span=051, se=F) +
  theme_classic() +
  geom_vline(xintercept=c(1700,1800,1900,2000), linetype=3) +
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14)) +
  theme (axis.text.x = element_text(color = "black", size = 12, angle = -45, hjust = .5, vjust =0.2),
         axis.text.y = element_text(color = "black", size = 12),
         axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5),
         legend.position = 'n')
         # plot.margin = margin(l = 55))


ggarrange(g1, g3, nrow=2, heights=c(1,1), labels=c('a','b'))


