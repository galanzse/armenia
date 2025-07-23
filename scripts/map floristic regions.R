

# CREATE AND PLOT MAP OF FLORISTICS REGIONS


library(terra)
library(rnaturalearth)
library(tidyverse)
library(readxl)



# df for plotting: set right names and calculate richness
df_florreg <- read_excel("data/alien_flora_armenia.xlsx", sheet="floristic regions") %>% dplyr::select(!species)
df_florreg <- data.frame(region = names(colSums(df_florreg, na.rm=T)), richness = colSums(df_florreg, na.rm=T))
rownames(df_florreg) <- NULL
df_florreg$code <- substr(df_florreg$region, 1, 2) %>% as.numeric()
df_florreg$region <- gsub(" ", "", substr(df_florreg$region, 5, 20))

# armenia borders
armenia_boder <- ne_countries(country='Armenia', scale=10)[1] %>% vect() %>% as.polygons()

# floristic regions
floreg_polygons <- vect('maps/Floristic regions of Armenia (polygons).kml')
floreg_polygons$Description <- NULL
names(floreg_polygons) <- 'region'
floreg_polygons$code <- df_florreg$code
floreg_polygons$richness <- df_florreg$richness



# polygons to raster to avoid overlap
floreg_raster <- rast('C:/Users/javie/OneDrive/ACADEMICO/proyectos/scleria/data/wc2.1_30s_elev/wc2.1_30s_elev.tif') %>% # base map
  terra::crop(armenia_boder); floreg_raster[] <- NA 

floreg_raster <- rasterize(x=floreg_polygons, y=floreg_raster, fun='min', field='region', update=T) %>% # rasterize
  terra::crop(armenia_boder, mask=T)

# rasterize does so by the alphabetical order of the categories, so vect and add richness
floreg_polygons2 <- as.polygons(floreg_raster)
floreg_polygons2$richness <-  df_florreg$richness[order(df_florreg$region, decreasing=F)]
  

# plot
plot(floreg_polygons2, 'richness', col=map.pal('reds', 20), type='continuous', main='Invasive species richness')
lines(floreg_polygons2, col='black', lwd=2)
# lines(floreg_polygons, col='orange', lwd=2)
lines(armenia_boder, col='black', lwd=3)


