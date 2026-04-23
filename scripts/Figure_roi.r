

# Figura del area de estudio


source('scripts/2.1_import_climate_data.r')


# llamo el plot
plot(v_madrid, asp=1)


# escala
sbar(d=30000, xy="bottomleft", type="bar", label=c('0 km', '15 km', '30 km'), cex=0.8)


# estaciones de polen
points(v_pollen_stations)
text(geom(v_pollen_stations)[,'x']+2000, geom(v_pollen_stations)[,'y']+2000, cex=0.6, labels=v_pollen_stations$codigo)


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


# puntos de la AEMET
points(aemet_points, col='blue', pch=8)
aemet_points$CODIGO = c('ARAN', 'SANS', 'POZU', 'RETI', 'ALPE', 'CIUD', 'TORR', 'RETI', 'GETA')
text(geom(aemet_points)[,'x']-3000, geom(aemet_points)[,'y']-3000, col='blue', cex=0.6, labels=aemet_points$CODIGO)


# leyenda
legend(x=365000, y=4560000, 
       legend = c("Pollen traps", "Pollen traps (previous location)", "Climate stations"),
       col = c("black","red","blue"),
       pch = c(16, 3, 8),
       cex = 0.8,   # reduce tamaño del texto
       bty = "n")


