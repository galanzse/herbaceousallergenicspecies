

# Importar datos de estaciones de polen


library(tidyverse)
library(readxl)
library(terra)
library(mapSpain)


pollen_stations <- read_excel('data/pollen/captadores/captadores_polen.xlsx')

# retain relevant variables
pollen_stations <- pollen_stations %>% dplyr::select("codigo", "nombre", "direccion_localidad", "fecha_inicio_mediciones", "x_epsg25830", "y_epsg25830")

colnames(pollen_stations)[colnames(pollen_stations)%in%c("x_epsg25830","y_epsg25830")] <- c('x','y')

# transform into spat_vector
v_pollen_stations <- pollen_stations %>% terra::vect(geom=c("x", "y"), crs='epsg:25830')

# import madrid shapefile
v_madrid <- mapSpain:::esp_get_ccaa('madrid',epsg="4326") %>% terra::vect() %>% project('epsg:25830')

plot(v_madrid, main='Pollen stations in Madrid')
points(v_pollen_stations)
text(geom(v_pollen_stations)[,'x']+2000, geom(v_pollen_stations)[,'y']+2000, cex=0.7, labels=v_pollen_stations$codigo)



# identificar cambios en las estaciones
cambios_captadores <- read_excel("data/pollen/captadores/cambios_captadores.xlsx") %>% as.data.frame()
v_cambios_captadores = vect(cambios_captadores, geom=c('x_25830', 'y_25830'), crs='epsg:25830')

points(v_cambios_captadores[v_cambios_captadores$status=='before',], col='red', pch=3)


line <- vect(rbind(c(468704.4, 4481745), c(468594.4, 4481147)), type="lines", crs=crs(v_cambios_captadores))
lines(line, col = "red", lwd = 2)
line <- vect(rbind(c(447258.8, 4487530), c(444727.7, 4488187)), type="lines", crs=crs(v_cambios_captadores))
lines(line, col = "red", lwd = 2)
line <- vect(rbind(c(448694.6, 4431441), c(448913.3, 4431454)), type="lines", crs=crs(v_cambios_captadores))
lines(line, col = "red", lwd = 2)
line <- vect(rbind(c(440683.4, 4475366), c(440985.5, 4472663)), type="lines", crs=crs(v_cambios_captadores))
lines(line, col = "red", lwd = 2)
line <- vect(rbind(c(453100.0, 4475498), c(452024.6, 4475222)), type="lines", crs=crs(v_cambios_captadores))
lines(line, col = "red", lwd = 2)
line <- vect(rbind(c(415201.0, 4498821), c(415324.3, 4500865)), type="lines", crs=crs(v_cambios_captadores))
lines(line, col = "red", lwd = 2)
rm(line)


data.frame(code = v_cambios_captadores[v_cambios_captadores$status=='before']$code, 
           distance = terra::nearest(v_cambios_captadores[v_cambios_captadores$status=='before'],
                                     v_cambios_captadores[v_cambios_captadores$status=='after'])$distance,
           year=v_cambios_captadores[v_cambios_captadores$status=='before']$shift_year)

rm(cambios_captadores, v_cambios_captadores)


