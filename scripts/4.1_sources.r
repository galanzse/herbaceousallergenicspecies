

# CORINE LAND COVER (2017-2018)


# Saco un figura como la Fig. 4 Picornell et al 2023: diferencias entre estaciones y cambios temporales

# Fig. 4 Picornell et al 2023: Spearman correlation and significance of the index calculated for each land use type within a radius of 5, 10, 22 km


library(tidyverse)
library(terra)
library(readxl)
library(vegan)
library(landscapemetrics)

source('scripts/0.1_pollen_stations.r')


# POLLEN DATA ####

pollen = read.csv("results/parametros.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, sm.tt) %>%
  subset(method=='percentage') %>%
  mutate(method=NULL) %>%
  group_by(type, site) %>%
  summarise(sm.tt_2000 = mean(sm.tt[seasons %in% 1999:2001], na.rm=TRUE),
            sm.tt_2006 = mean(sm.tt[seasons %in% 2005:2007], na.rm=TRUE),
            sm.tt_2012 = mean(sm.tt[seasons %in% 2011:2013], na.rm=TRUE),
            sm.tt_2018 = mean(sm.tt[seasons %in% 2017:2019], na.rm=TRUE),
            .groups = "drop") %>%
  pivot_longer(cols=c(sm.tt_2000, sm.tt_2006, sm.tt_2012, sm.tt_2018), names_to='año', values_to='sm.tt')

pollen$año[pollen$año=='sm.tt_2000'] = 2000
pollen$año[pollen$año=='sm.tt_2006'] = 2006
pollen$año[pollen$año=='sm.tt_2012'] = 2012
pollen$año[pollen$año=='sm.tt_2018'] = 2018
pollen$año = as.numeric(pollen$año)



# EXTRAER DATOS PARA CADA ESTACION ####

# LAND USE
lu_spatial_dif = expand_grid(site = unique(pollen$site),
                             año = c(2000, 2006, 2012, 2018))

# CORINE reclassified: CORINErecl
lu_spatial_dif$a_urban = NA
lu_spatial_dif$a_nonirrigated_arable_land = NA
lu_spatial_dif$a_permanently_irrigated_land = NA
lu_spatial_dif$a_permanent_crops = NA
lu_spatial_dif$a_pastures = NA
lu_spatial_dif$a_agricultural_mosaic = NA
lu_spatial_dif$a_forests = NA
lu_spatial_dif$a_natural_grasslands = NA
lu_spatial_dif$a_shrubland = NA
lu_spatial_dif$a_sclerophyllous_forests = NA
lu_spatial_dif$ed = NA
lu_spatial_dif$pd = NA
lu_spatial_dif$shdi = NA

# loop
for (i in 1:nrow(lu_spatial_dif)) {
  
  # sitio + buffer 10km
  st = v_pollen_stations[v_pollen_stations$codigo==lu_spatial_dif$site[i],] %>%
    project(crs(CORINErecl)) %>% buffer(10000)
  
  # recorto CLC a roi
  clcxst = CORINErecl %>% terra::crop(st, mask=T)

  # selecciono año
  clcxst = clcxst[[grep(lu_spatial_dif$año[i], names(clcxst))]]

  # CORINErecl
  lvl1 = levels(clcxst)[[1]]; colnames(lvl1) = c('class','name')
  lvl1 = merge(lvl1, lsm_c_ca(clcxst)[,c('class','value')])
  
  # class metrics
  lu_spatial_dif$a_urban[i] = ifelse(length(lvl1$value[lvl1$name=='urban']) != 0, lvl1$value[lvl1$name=='urban'], 0)
  lu_spatial_dif$a_nonirrigated_arable_land[i] = ifelse(length(lvl1$value[lvl1$name=='nonirrigated arable land']) != 0, lvl1$value[lvl1$name=='nonirrigated arable land'], 0)
  lu_spatial_dif$a_permanently_irrigated_land[i] = ifelse(length(lvl1$value[lvl1$name=='permanently irrigated land']) != 0, lvl1$value[lvl1$name=='permanently irrigated land'], 0)
  lu_spatial_dif$a_permanent_crops[i] = ifelse(length(lvl1$value[lvl1$name=='permanent crops']) != 0, lvl1$value[lvl1$name=='permanent crops'], 0)
  lu_spatial_dif$a_pastures[i] = ifelse(length(lvl1$value[lvl1$name=='pastures']) != 0, lvl1$value[lvl1$name=='pastures'], 0)
  lu_spatial_dif$a_agricultural_mosaic[i] = ifelse(length(lvl1$value[lvl1$name=='agricultural mosaic']) != 0, lvl1$value[lvl1$name=='agricultural mosaic'], 0)
  lu_spatial_dif$a_forests[i] = ifelse(length(lvl1$value[lvl1$name=='forests']) != 0, lvl1$value[lvl1$name=='forests'], 0)
  lu_spatial_dif$a_natural_grasslands[i] = ifelse(length(lvl1$value[lvl1$name=='natural grasslands']) != 0, lvl1$value[lvl1$name=='natural grasslands'], 0)
  lu_spatial_dif$a_shrubland[i] = ifelse(length(lvl1$value[lvl1$name=='shrubland']) != 0, lvl1$value[lvl1$name=='shrubland'], 0)
  lu_spatial_dif$a_sclerophyllous_forests[i] = ifelse(length(lvl1$value[lvl1$name=='sclerophyllous forests']) != 0, lvl1$value[lvl1$name=='sclerophyllous forests'], 0)

  # landscape metrics
  lu_spatial_dif$ed[i] = lsm_l_ed(clcxst[[grep(lu_spatial_dif$año[i], names(clcxst))]])$value
  lu_spatial_dif$pd[i] = lsm_l_pd(clcxst[[grep(lu_spatial_dif$año[i], names(clcxst))]])$value
  lu_spatial_dif$shdi[i] = lsm_l_shdi(clcxst[[grep(lu_spatial_dif$año[i], names(clcxst))]])$value

  # progress
  print(paste(round(i/nrow(lu_spatial_dif),4)*100, '%'))
  
}





# DIFERENCIAS ESPACIALES EN USOS DE SUELO ####

# unimos polen con lu
CORINE_x_Polen = merge(lu_spatial_dif, pollen, by=c('site','año')) %>%
  pivot_longer(3:15, names_to='landuse', values_to='value')

# hacemos correlaciones
cor_CxP = CORINE_x_Polen %>% group_by(landuse, type, año) %>%
  summarise(rho = cor.test(sm.tt, value, method='spearman')$estimate,
            pval = cor.test(sm.tt, value, method='spearman')$p.value)

cor_CxP$sig <- ifelse(cor_CxP$pval < 0.05, "sig.", "non sig.")

cor_CxP <- cor_CxP %>%
  mutate(
    label = ifelse(sig == "sig.", "***", ""),
    rho = as.numeric(rho)
  )

ggplot(cor_CxP, aes(x = landuse, y = type, fill = rho)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label), size = 3) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    na.value = "grey90"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  labs(fill = "rho", x = NULL, y = "Type")

# amar destaca en zonas cultivadas
# arte, plan y rumi muestras patrones dificiles de interpretar
# rume destaca en bosques y zonas agroforestales


