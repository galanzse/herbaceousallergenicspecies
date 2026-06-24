

# Identificamos habitats relevantes (datos de 2004-2005)
# https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/atlas_manual_habitats_espanioles.html


library(tidyverse)
library(terra)
library(readxl)
library(rgbif)
library(vegan)

source('scripts/0.1_pollen_stations.r')


# cartografia de habitats ####

# https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/index_atlas_manual_habitats.html
# 1:50000

t0 = '/Users/jesusrojo/Library/CloudStorage/OneDrive-Personal/ACADEMICO/proyectos/fuentes_palinocam/data/habitats/'
# t0 = 'C:/Users/javie/OneDrive/ACADEMICO/proyectos/fuentes_palinocam/data/habitats/'
habitats = rbind(vect(paste0(t0, 'INB_HABITAT05_2005_tcm30-199593/INB_Habitat05_ETRS89_H30.shp')), 
                 vect(paste0(t0, 'INB_HABITAT19_2005_tcm30-199621/INB_Habitat19_ETRS89_H30.shp')), 
                 vect(paste0(t0, 'INB_HABITAT28_2005_tcm30-199640/INB_Habitat28_ETRS89_H30.shp')), 
                 vect(paste0(t0, 'INB_HABITAT40_2005_tcm30-199664/INB_Habitat40_ETRS89_H30.shp')),
                 vect(paste0(t0, 'INB_HABITAT45_2005_tcm30-199674/INB_Habitat45_ETRS89_H30.shp')))

# recortamos al area de interes
roi = terra::buffer(v_pollen_stations, width=22000) %>% aggregate()
habitats = habitats %>% crop(roi)

# datos asociados
habitats_data <- read_excel("data/habitats_data.xlsx")

# un poligono puede tener varios habitats
habitats <- merge(habitats[, c("HAB_LAY")], habitats_data)
habitats$AREA_KM2 = expanse(habitats, unit="km", transform=TRUE)

# eliminamos poligonos muy grandes porque no aplican a nuestras especies herbaceas, queremos poligonos bien cartografiados
table(habitats$AREA_KM2<2)
habitats = habitats[habitats$AREA_KM2<2,]

# eliminamos los menos representados
table(habitats$PORCENTAJE <= 10)
habitats = habitats[habitats$PORCENTAJE>10,]



# observaciones de gbif ####

# # quiero ver que taxones suelen caer en que habitats
# 
# # AMAR key 3064
# # ARTE key 3120641
# # PLAN key 3189695
# # RUME key 2888942
# # URTI key 6639
# 
# # roi en WKT format
# gmt = 'POLYGON((-4.82 39.72, -2.78 39.72, -2.78 41.38, -4.82 41.38, -4.82 39.72 ))'
# 
# observations <- occ_search(taxonKey=c(3064, 3120641, 3189695, 2888942, 6639),
#                            hasCoordinate=T, geometry=gmt, limit=100000)
# 
# saveRDS(observations, 'data/gbif_data.rds')
# 
# observations = readRDS('data/gbif_data.rds')
# 
# # retengo las columnas que me interesan y hago rbind
# cl = c("family", "genus", "species", "decimalLatitude", "decimalLongitude", "basisOfRecord", "familyKey", "genusKey", "speciesKey")
# gbif_data = rbind(observations$`3064`$data[,cl], observations$`3120641`$data[,cl], observations$`3189695`$data[,cl], observations$`2888942`$data[,cl], observations$`6639`$data[,cl])
# 
# # añado info de TPP
# gbif_data$TPP = NA
# gbif_data$TPP[gbif_data$familyKey==3064] = 'AMAR'
# gbif_data$TPP[gbif_data$familyKey==6639] = 'URTI'
# gbif_data$TPP[gbif_data$genusKey==3120641] = 'ARTE'
# gbif_data$TPP[gbif_data$genusKey==3189695] = 'PLAN'
# gbif_data$TPP[gbif_data$genusKey==2888942] = 'RUME'
# 
# # guardo
# write.table(gbif_data, 'results/gbif_data.txt')
# gbif_data <- read.csv("results/gbif_data.txt", sep="")
# 
# # retengo TPP y coordenadas, y transformo
# v_gbif_data = vect(gbif_data, geom=c('decimalLongitude','decimalLatitude'), 'epsg:4326')[,'TPP'] %>%
#   terra::project(habitats)
# 
# # elimino puntos del mismo TPP que estan cerca
# thin_points <- function(v, dist) {
#   keep <- rep(TRUE, nrow(v))
#   
#   for (i in 1:(nrow(v) - 1)) {
#     if (keep[i]) {
#       d <- distance(v[i], v[(i+1):nrow(v)])
#       keep[(i+1):nrow(v)] <- keep[(i+1):nrow(v)] & (d > dist)
#     }
#   }
#   
#   return(v[keep])
# }
# 
# # separo por TPP
# groups <- split(v_gbif_data, v_gbif_data$TPP)
# 
# # spatial thinning: 200m
# thinned_list <- lapply(groups, thin_points, dist=200)
# 
# # merge
# v_thinned <- rbind(thinned_list$AMAR[,'TPP'], thinned_list$ARTE[,'TPP'], thinned_list$PLAN[,'TPP'], thinned_list$RUME[,'TPP'], thinned_list$URTI[,'TPP'])
# 
# writeVector(v_thinned, 'results/gbif_data_thinned.shp', overwrite=TRUE)



# cruzo habitats y gbif ####

# solo quiero hacer un analisis exploratorio por lo que la resolucion de habitats y precision de gbif me da igual

gbif_data_thinned <- vect('results/gbif_data_thinned.shp')

# añado id porque los poligonos solapan (mismo poligono pero cada capa es la proporcion de un habitat)
gbif_data_thinned$id = 1:nrow(gbif_data_thinned)

# extraigo y añado info
habitats_sum = terra::extract(habitats, gbif_data_thinned)
habitats_sum = merge(gbif_data_thinned, habitats_sum, by.x='id', by.y='id.y')
habitats_sum = as.data.frame(habitats_sum)

# frecuencias
habitats_results = table(habitats_sum$GENERICO, habitats_sum$TPP) %>% as.matrix()
habitats_results <- prop.table(habitats_results, margin=2) %>% as.data.frame() %>%
  pivot_wider(names_from=Var2, values_from=Freq)
colnames(habitats_results)[1] <- 'habitat'



# EXPLORATORIO ####


# MAPAS: habitats que retienen mas del 5% de observaciones x TPP
hab_AMAR = habitats[habitats$GENERICO %in% habitats_results$habitat[habitats_results$AMAR>0.07],]
hab_ARTE = habitats[habitats$GENERICO %in% habitats_results$habitat[habitats_results$ARTE>0.07],]
hab_PLAN = habitats[habitats$GENERICO %in% habitats_results$habitat[habitats_results$PLAN>0.07],]
hab_RUME = habitats[habitats$GENERICO %in% habitats_results$habitat[habitats_results$RUME>0.07],]
hab_URTI = habitats[habitats$GENERICO %in% habitats_results$habitat[habitats_results$URTI>0.07],]

par(mfrow=c(3,2))
plot(hab_AMAR, main='Habitats AMAR'); lines(v_madrid, col='blue')
plot(hab_ARTE, main='Habitats ARTE'); lines(v_madrid, col='blue')
plot(hab_PLAN, main='Habitats PLAN'); lines(v_madrid, col='blue')
plot(hab_RUME, main='Habitats RUME'); lines(v_madrid, col='blue')
plot(hab_URTI, main='Habitats URTI'); lines(v_madrid, col='blue')



# NMDS
mat <- as.data.frame(habitats_results)
rownames(mat) <- mat$habitat
mat <- mat[,-1]

# retenemos filas en que al menos un tipo sea mayor del 3%
mat_filtered <- mat[ apply(mat, 1, function(x) any(x >= 0.03)), ]

nmds <- metaMDS(mat_filtered, distance="bray", k=2, trymax=100)

par(mfrow=c(1,1))
plot(nmds, type="t", cex=0.6)



# habitats ~ APIn
pollen = read.csv("results/fenofases.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, sm.ps) %>%
  subset(method=='percentage' & seasons %in% c(2003:2007)) %>%
  mutate(method=NULL) %>%
  group_by(type, site) %>%
  summarise(APIn = mean(sm.ps, na.rm=T))
  
# retenemos las categorias mas frecuentes
habitats_filtered = habitats[habitats$GENERICO%in%rownames(mat_filtered),'GENERICO']

# vemos si hay poligonos solapantes
any(relate(habitats_filtered, habitats_filtered, "overlaps"))

# raserizamos
habitats_r <- rast(ext=ext(habitats_filtered), resolution=100, crs=crs(habitats_filtered))
habitats_r <- rasterize(habitats_filtered, habitats_r, field='GENERICO')
plot(habitats_r)

# extraemos valores
buf = terra::buffer(v_pollen_stations, width=22000)[,'codigo']
habitats_st =  terra::extract(x=habitats_r, y=buf, fun=table, na.rm=TRUE)
habitats_st$site = buf$codigo; habitats_st$ID = NULL

# merge polen y habitats
polen_x_habitat = merge(pollen, habitats_st, by='site') %>%
  pivot_longer(4:26, names_to='habitat', values_to='habitat_counts')

t0 = 'URTI'
ggplot(aes(y=APIn, x=log(habitat_counts), color=site), data=polen_x_habitat[polen_x_habitat$type==t0,]) +
  geom_point() +
  theme_bw() +
  labs(x='number of cells in buffer', y=paste('APIn', t0)) +
  theme(legend.position='top') +
  guides(color=guide_legend(nrow=1, title=NULL)) +
  facet_wrap(~habitat, scales='free_x')

polen_x_habitat %>%
  group_by(type, habitat) %>%
  summarise(rho = cor.test(APIn, habitat_counts, method='spearman')$estimate,
            pval = cor.test(APIn, habitat_counts, method='spearman')$p.value) %>%
  subset(pval<0.05)


