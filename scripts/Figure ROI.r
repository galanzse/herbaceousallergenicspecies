

# Figura del area de estudio


source('scripts/2.1_import_climate_data.r')
library(rnaturalearth)


# landuses
CLC_2018 = rast('results/CORINErecl$clc2018.tif') %>% project(v_madrid)

# buffer del estudio
CLC_2018 = CLC_2018 %>% crop(aggregate(buffer(v_pollen_stations, 10000)), mask=T)

# llamo el plot
plot(CLC_2018, col=c('#fee6ce', 'lightgreen', '#d9d9d9'), asp=1)

# añado lineas
lines(v_madrid)

# escala
sbar(d=20000, xy="bottomleft", type="bar", label=c('0 km', '10 km', '20 km'), cex=0.8)

# estaciones de polen
points(v_pollen_stations)
text(geom(v_pollen_stations)[,'x']+2000, geom(v_pollen_stations)[,'y']+2000, cex=0.6, labels=v_pollen_stations$codigo)

# puntos de la AEMET
points(aemet_points, col='blue', pch=8)
aemet_points$CODIGO = c('ARAN', 'SANS', 'POZU', 'RETI', 'ALPE', 'CIUD', 'TORR', 'RETI', 'GETA')
text(geom(aemet_points)[,'x']-3000, geom(aemet_points)[,'y']-3000, col='blue', cex=0.6, labels=aemet_points$CODIGO)

# leyenda
par(xpd = NA)
legend(x=479500, y=4499000, 
       legend = c("Pollen traps", "Climate stations"),
       col = c("black","blue"),
       pch = c(16, 3, 8),
       cex = 0.8,   # reduce tamaño del texto
       bty = "n")


# inset plot
esp <- ne_countries(country = "Spain", scale = "medium", returnclass = "sf") %>%
  vect() %>% crop(ext(-10,5,35,45)) %>% project(CLC_2018)

par(fig = c(0.52, 0.98, 0.12, 0.58), new = TRUE)

plot(esp, col = "white", border = "black",  lwd = 2, axes = FALSE, ann = FALSE, bty = "n")
lines(v_madrid)
lines(ext(CLC_2018), col = "red")


