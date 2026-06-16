

# Importar datos de estaciones de polen


library(tidyverse)
library(readxl)
library(terra)
library(mapSpain)


pollen_stations <- read_excel('data/pollen/captadores/captadores_polen.xlsx')

# me cargo Subiza
pollen_stations = pollen_stations[pollen_stations$codigo!='ALER',]

# retain relevant variables
pollen_stations <- pollen_stations %>% dplyr::select("codigo", "nombre", "direccion_localidad", "fecha_inicio_mediciones", "x_epsg25830", "y_epsg25830")

colnames(pollen_stations)[colnames(pollen_stations)%in%c("x_epsg25830","y_epsg25830")] <- c('x','y')

# transform into spat_vector
v_pollen_stations <- pollen_stations %>% terra::vect(geom=c("x", "y"), crs='epsg:25830')

# # roi
# roi = buffer(v_pollen_stations, width=30000) %>% aggregate() %>% project('epsg:4326')
# writeVector(roi, 'results/roi.shp')

# import madrid shapefile
v_madrid <- mapSpain:::esp_get_ccaa('madrid',epsg="4326") %>% terra::vect() %>% project('epsg:25830')

plot(v_madrid, main='Pollen stations in Madrid')
points(v_pollen_stations)
text(geom(v_pollen_stations)[,'x']+2000, geom(v_pollen_stations)[,'y']+2000, cex=0.7, labels=v_pollen_stations$codigo)


