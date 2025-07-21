

# MAP OF FLORISTICS REGIONS BY OVERLAPPING IN GOOGLE EARTH


library(terra)
library(rnaturalearth)


# armenia
armenia_map <- ne_countries(country='Armenia', scale=10)[1] %>% vect() %>% as.polygons()

# floristic regions
floristic_regions <- vect('maps/Floristic regions of Armenia (polygons).kml')
floristic_regions$Name <- c('Upper Akhurian', 'Shirak', 'Lori', 'Idjevan', 'Aparan', 'Sevan',
                            'Areguni', 'Yerevan', 'Darelegis', 'North Zangezur', 'South Zangezur', 'Megri')
# crop and plot
plot(floristic_regions)
lines(armenia_map, col='red')

# rasterize
rast1 <- rast('C:/Users/javie/OneDrive/ACADEMICO/proyectos/scleria/data/wc2.1_30s_elev/wc2.1_30s_elev.tif') %>% terra::crop(armenia_map)
rast1[] <- NA 

rast_flor <- rasterize(x=floristic_regions, y=rast1, fun='min', field='Name', update=T) %>% terra::crop(armenia_map, mask=T)
plot(rast_flor)
lines(armenia_map)


