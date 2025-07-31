

# EXPLORE DIVERISTY PATTERNS AMONG REGIONS: OCCURRENCE DATA ONLY FOR INVASIVE SPECIES!!

# al cruzar todas las bases de datos (rasgos, filogenia, abundancias) se quedan 20/30 especies fuera,
# esto habria que corregirlo si queremos hacer un articulo

# se aprecia un patron de cambio de riqueza total (tambien funcional y filogenetica) con la elevacion

# al separar los indices por status se ven diferencias en las cwm,
# pero no se aprecian respuestan claras (convergentes o divergentes) entre grupos a lo largo de un gradiente de elevacion


# floristic regions
source('scripts/map floristic regions.R')


# library(readxl)
# library(tidyverse)
library(ape)
library(BAT)
library(fundiversity)
library(picante)
# library(funspace)
# library(terra)
# library(rnaturalearth)



# import species
species_df <- read_excel("data/alien_flora_armenia.xlsx", sheet="Sheet3") %>%
  dplyr::select(species, family, life_form, life_cycle, growth_form, status) 
species_df$species <- gsub(" ", "_", species_df$species)



# traits: impute manually, import as many species as possible, impute automatically, retain invasives
CSR_means_wide <- read_excel("results/CSR_means_wide.xlsx")
# traits with at least 70% observations: CSR
colSums(!(is.na(CSR_means_wide)))/nrow(CSR_means_wide)*100



# tree
load("C:/Users/javie/OneDrive/ACADEMICO/proyectos/armenia/results/tree.Rdata")



# impute using tree
table(gsub(" ", "_", CSR_means_wide$species) %in% tree$scenario.3$tip.label)
sp1 <- gsub(" ", "_", CSR_means_wide$species)[gsub(" ", "_", CSR_means_wide$species)%in%tree$scenario.3$tip.label]
mytree <- keep.tip(phy=tree$scenario.3, tip=sp1)

mat_CSR_means_wide <- as.matrix(CSR_means_wide[,-1])
rownames(mat_CSR_means_wide) <- gsub(" ", "_", CSR_means_wide$species)
CSR_means_imputed <- impute(traits=mat_CSR_means_wide, phylo=mytree)
CSR_means_imputed <- CSR_means_imputed$imputed



# floristic regions: matrix
floristic_reg <- read_excel("data/alien_flora_armenia.xlsx",  sheet = "floristic regions")
floristic_reg$species <- gsub(" ", "_", floristic_reg$species)
colnames(floristic_reg)[-1] <- c("Upper Akhuryan", "Shirak", "Lori", "Ijevan", "Aparan", "Sevan", "Areguni", "Yerevan", "Darelegis", "Northern Zangezur", "Southern Zangezur", "Meghri")

floristic_reg_long <- floristic_reg %>% pivot_longer(!species, names_to = "flor_region", values_to = "count") %>% na.omit()

mat_floristic_reg <- as.data.frame(floristic_reg)
rownames(mat_floristic_reg) <- mat_floristic_reg$species; mat_floristic_reg$species <- NULL
mat_floristic_reg[is.na(mat_floristic_reg)] <- 0

# floristic regions: polygons
floreg_polygons2$name <- unique(floristic_reg_long$flor_region)[order(unique(floristic_reg_long$flor_region), decreasing=F)]



# retain common species
sp1 <- species_df$species %>%
  intersect(rownames(CSR_means_imputed)) %>%
  intersect(tree$scenario.3$tip.label)

mat_floristic_reg <- mat_floristic_reg[sp1,]



# species dataframe
species_df <- species_df[species_df$species%in%sp1,]
table(species_df$status)

# traits table
CSR_means_imputed <- CSR_means_imputed[species_df$species,]

# fix tree tips
mytree <- keep.tip(tree$scenario.3, species_df$species)



# import climatic data (2.5 minutes ~21 km2 at the equator)
armenia_shp <- ne_countries(country='Armenia', scale=10)[1] %>% vect() %>% as.polygons()
worldclim_crop <- rast(list.files('C:/Users/javie/Desktop/world_rasters/wc2.1_2.5m', full.names=T)) %>%
  crop(armenia_shp, mask=T)
# BIO1 Annual Mean Temperature, BIO6 Min Temperature of Coldest Month, BIO7 Temperature Annual Range (BIO5-BIO6)
# BIO12 Annual Precipitation, BIO13 Precipitation of Wettest Month, BIO15 Precipitation Seasonality (Coefficient of Variation)

# elevation
elevation_crop <- rast('C:/Users/javie/Desktop/world_rasters/wc2.1_2.5m_elev.tif') %>%
  crop(armenia_shp, mask=T)

# HPD
hpd_crop <- rast('C:/Users/javie/Desktop/world_rasters/Global_2020_PopulationDensity30sec_GPWv4.tiff') %>%
  crop(armenia_shp, mask=T)

# HFI
hfi_crop <- rast('C:/Users/javie/Desktop/world_rasters/hfp2013_merisINT.tif') %>%
  crop(terra::project(armenia_shp, rast('C:/Users/javie/Desktop/world_rasters/hfp2013_merisINT.tif')), mask=T) %>%
  terra::project(hpd_crop)



# extract predictors + indexes total
pred_x_regions <- data.frame(region=unique(floristic_reg_long$flor_region),
                            BIO1=NA, BIO6=NA, BIO7=NA, BIO12 =NA, BIO13=NA, BIO15=NA,
                            elevation=NA, HPD=NA, HFI=NA,
                            richness=NA,
                            FunRich=NA, FunDis=NA, PhyRich=NA, PhyDiv=NA,
                            SLA_cwm=NA, height_cwm=NA, seed_cwm=NA)

for (i in 1:nrow(pred_x_regions)) {
  
  # polygon
  reg1 <- floreg_polygons2[floreg_polygons2$name==ind_x_regions$region[i]]
  
  
  # climate
  pred_x_regions$BIO1[i] <- worldclim_crop$wc2.1_2.5m_bio_1 %>% crop(reg1, mask=T) %>% as.data.frame() %>% deframe() %>% mean()
  pred_x_regions$BIO6[i] <- worldclim_crop$wc2.1_2.5m_bio_6 %>% crop(reg1, mask=T) %>% as.data.frame() %>% deframe() %>% mean()
  pred_x_regions$BIO7[i] <- worldclim_crop$wc2.1_2.5m_bio_7 %>% crop(reg1, mask=T) %>% as.data.frame() %>% deframe() %>% mean()
  pred_x_regions$BIO12[i] <- worldclim_crop$wc2.1_2.5m_bio_12 %>% crop(reg1, mask=T) %>% as.data.frame() %>% deframe() %>% mean()
  pred_x_regions$BIO13[i] <- worldclim_crop$wc2.1_2.5m_bio_13 %>% crop(reg1, mask=T) %>% as.data.frame() %>% deframe() %>% mean()
  pred_x_regions$BIO15[i] <- worldclim_crop$wc2.1_2.5m_bio_15 %>% crop(reg1, mask=T) %>% as.data.frame() %>% deframe() %>% mean()

  # other abiotic
  pred_x_regions$elevation[i] <- elevation_crop %>% crop(reg1, mask=T) %>% as.data.frame() %>% deframe() %>% mean()
  pred_x_regions$HPD[i] <- hpd_crop %>% crop(reg1, mask=T) %>% as.data.frame() %>% deframe() %>% mean()
  pred_x_regions$HFI[i] <- hfi_crop %>% crop(reg1, mask=T) %>% as.data.frame() %>% deframe() %>% mean()
  
  
  # indexes
  mat_sp <-  mat_floristic_reg %>% as.data.frame() %>% dplyr::select(ind_x_regions$region[i]) %>% as.matrix() %>% t()
  v_sp <-  colnames(mat_sp)[which(mat_sp>0)]

  pred_x_regions$richness[i] = v_sp %>% length()
  pred_x_regions$FunRich[i] <- fd_fric(CSR_means_imputed, mat_sp, stand=TRUE)$FRic
  pred_x_regions$FunDis[i] <- fd_fdis(CSR_means_imputed, mat_sp)$FDis
  pred_x_regions$PhyRich[i] <- pd(mat_sp, mytree, include.root=TRUE)$PD
  pred_x_regions$PhyDiv[i] <- mpd(mat_sp, cophenetic(mytree), abundance.weighted=FALSE)
  pred_x_regions$SLA_cwm[i] <- CSR_means_imputed[v_sp,'SLA'] %>% mean()
  pred_x_regions$height_cwm[i] <- CSR_means_imputed[v_sp,'height_vegetative'] %>% mean()
  pred_x_regions$seed_cwm[i] <- CSR_means_imputed[v_sp,'seed_mass'] %>% mean()

}

head(pred_x_regions)
# BIO1 Annual Mean Temperature, BIO6 Min Temperature of Coldest Month, BIO7 Temperature Annual Range (BIO5-BIO6)
# BIO12 Annual Precipitation, BIO13 Precipitation of Wettest Month, BIO15 Precipitation Seasonality (Coefficient of Variation)

# al aumentar la elevacion: disminuye richness (==PhyRich) y FunRich, aumenta SLA y disminuye seed mass
# al aumentar la presion antropica: aumenta richness (==PhyRich)
ggplot(aes(x=BIO1, y=seed_cwm), data=pred_x_regions) +
  geom_point() +
  theme_classic()



# 2/ indexes per status

status_x_regions <- list()

for (s in unique(species_df$status)) {
  
  # extract predictors + indexes total
  pred_x_regions_sta <- data.frame(region=unique(floristic_reg_long$flor_region),
                                   richness=NA,
                                   FunRich=NA, FunDis=NA, PhyRich=NA, PhyDiv=NA,
                                   SLA_cwm=NA, height_cwm=NA, seed_cwm=NA)
  pred_x_regions_sta$status <- s

  for (i in 1:nrow(pred_x_regions_sta)) {
    
    # indexes
    s_sta <- species_df[species_df$status==s,] %>% dplyr::select(species) %>% deframe()
    mat_sp <-  mat_floristic_reg[s_sta,] %>% as.data.frame() %>% dplyr::select(ind_x_regions$region[i]) %>% as.matrix() %>% t()
    v_sp <-  colnames(mat_sp)[which(mat_sp>0)]
    
    pred_x_regions_sta$richness[i] = v_sp %>% length()
    pred_x_regions_sta$FunRich[i] <- fd_fric(CSR_means_imputed, mat_sp, stand=FALSE)$FRic
    pred_x_regions_sta$FunDis[i] <- fd_fdis(CSR_means_imputed, mat_sp)$FDis
    pred_x_regions_sta$PhyRich[i] <- pd(mat_sp, mytree, include.root=TRUE)$PD
    pred_x_regions_sta$PhyDiv[i] <- mpd(mat_sp, cophenetic(mytree), abundance.weighted=FALSE)
    pred_x_regions_sta$SLA_cwm[i] <- CSR_means_imputed[v_sp,'SLA'] %>% mean()
    pred_x_regions_sta$height_cwm[i] <- CSR_means_imputed[v_sp,'height_vegetative'] %>% mean()
    pred_x_regions_sta$seed_cwm[i] <- CSR_means_imputed[v_sp,'seed_mass'] %>% mean()
    
  }
  
  status_x_regions[[s]] <- pred_x_regions_sta

}

# collapse list
df_status_x_regions <- do.call(rbind, status_x_regions)
rownames(df_status_x_regions) <- NULL
# add elevation as proxy of climate pore xploratory analyses
status_x_regions <- merge(df_status_x_regions, pred_x_regions[,c('region','elevation','HFI')])

# exploratory
head(status_x_regions)
ggplot(aes(x=elevation, y=log(FunRich), colour=status, group=status), data=status_x_regions) +
  geom_smooth(method='loess', span=051, se=F) +
  geom_point() +
  theme_classic()

temp <- status_x_regions %>% dplyr::select(SLA_cwm, height_cwm, seed_cwm, status) %>%
  pivot_longer(1:3, names_to='trait', values_to='value') %>% na.omit()
ggplot(aes(x=status, y=log(value), colour=status), data=temp) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position='top', legend.title=element_blank()) +
  facet_wrap(~trait, scales='free')


