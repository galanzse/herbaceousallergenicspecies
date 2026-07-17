

# ES 10KM UN RADIO INTERESANTE PARA EXTRAER INFO DE TIPOS DE SUELO?

# Mirar justificación de Picornell et al (2023)

# Exploro si la relación entre buferes es redundante
# Pruebo 5, 10, 15 y 20 km


library(tidyverse)
library(terra)
library(readxl)
library(vegan)
library(landscapemetrics)

source('scripts/0.1_pollen_stations.r')
source('scripts/4.0_CLC reclassification.r')


# extraer datos para cada estacion, año y buffer
buff_imp = expand_grid(site = unique(pollen_stations$codigo),
                       año = c(2000, 2006, 2012, 2018),
                       buffer = c(5, 10, 15, 20))

# definimos las categorias de usos de suelo
buff_imp$a_urban = NA
buff_imp$a_nonirrigated_arable_land = NA
buff_imp$a_permanently_irrigated_land = NA
buff_imp$a_permanent_crops = NA
buff_imp$a_pastures = NA
buff_imp$a_agricultural_mosaic = NA
buff_imp$a_forests = NA
buff_imp$a_natural_grasslands = NA
buff_imp$a_shrubland = NA
buff_imp$a_sclerophyllous_forests = NA
buff_imp$ed = NA
buff_imp$pd = NA
buff_imp$shdi = NA

# loop
for (i in 1:nrow(buff_imp)) {
  
  # sitio + buffer 10km
  st = v_pollen_stations[v_pollen_stations$codigo==buff_imp$site[i],] %>%
    project(crs(CORINErecl)) %>% buffer(buff_imp$buffer[i]*1000)
  
  # recorto CLC a roi
  clcxst = CORINErecl %>% terra::crop(st, mask=T)
  
  # selecciono año
  clcxst = clcxst[[grep(buff_imp$año[i], names(clcxst))]]
  
  # CORINErecl
  lvl1 = levels(clcxst)[[1]]; colnames(lvl1) = c('class','name')
  lvl1 = merge(lvl1, lsm_c_ca(clcxst)[,c('class','value')])
  
  # class metrics
  buff_imp$a_urban[i] = ifelse(length(lvl1$value[lvl1$name=='urban']) != 0, lvl1$value[lvl1$name=='urban'], 0)
  buff_imp$a_nonirrigated_arable_land[i] = ifelse(length(lvl1$value[lvl1$name=='nonirrigated arable land']) != 0, lvl1$value[lvl1$name=='nonirrigated arable land'], 0)
  buff_imp$a_permanently_irrigated_land[i] = ifelse(length(lvl1$value[lvl1$name=='permanently irrigated land']) != 0, lvl1$value[lvl1$name=='permanently irrigated land'], 0)
  buff_imp$a_permanent_crops[i] = ifelse(length(lvl1$value[lvl1$name=='permanent crops']) != 0, lvl1$value[lvl1$name=='permanent crops'], 0)
  buff_imp$a_pastures[i] = ifelse(length(lvl1$value[lvl1$name=='pastures']) != 0, lvl1$value[lvl1$name=='pastures'], 0)
  buff_imp$a_agricultural_mosaic[i] = ifelse(length(lvl1$value[lvl1$name=='agricultural mosaic']) != 0, lvl1$value[lvl1$name=='agricultural mosaic'], 0)
  buff_imp$a_forests[i] = ifelse(length(lvl1$value[lvl1$name=='forests']) != 0, lvl1$value[lvl1$name=='forests'], 0)
  buff_imp$a_natural_grasslands[i] = ifelse(length(lvl1$value[lvl1$name=='natural grasslands']) != 0, lvl1$value[lvl1$name=='natural grasslands'], 0)
  buff_imp$a_shrubland[i] = ifelse(length(lvl1$value[lvl1$name=='shrubland']) != 0, lvl1$value[lvl1$name=='shrubland'], 0)
  buff_imp$a_sclerophyllous_forests[i] = ifelse(length(lvl1$value[lvl1$name=='sclerophyllous forests']) != 0, lvl1$value[lvl1$name=='sclerophyllous forests'], 0)
  
  # landscape metrics
  buff_imp$ed[i] = lsm_l_ed(clcxst[[grep(buff_imp$año[i], names(clcxst))]])$value
  buff_imp$pd[i] = lsm_l_pd(clcxst[[grep(buff_imp$año[i], names(clcxst))]])$value
  buff_imp$shdi[i] = lsm_l_shdi(clcxst[[grep(buff_imp$año[i], names(clcxst))]])$value
  
  # progress
  print(paste(round(i/nrow(buff_imp),4)*100, '%'))
  
}


buff_imp_form = buff_imp %>% pivot_longer(4:16, names_to = 'landuse') %>%
  pivot_wider(names_from = 'buffer', values_from = 'value')

colnames(buff_imp_form)[4:7] = c('5km', '10km', '15km','20km')

panel.cor <- function(x, y, digits = 2, cex.cor = 1.2) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  r <- cor(x, y, use = "complete.obs", method = "pearson")
  txt <- formatC(r, digits = digits, format = "f")
  
  text(0.5, 0.5, txt, cex = cex.cor)
}

pairs(
  buff_imp_form[, 4:7],
  upper.panel = panel.cor
)


