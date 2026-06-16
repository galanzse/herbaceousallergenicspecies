

# ASOCIAR INCREMENTOS DE POLEN A VARIACIONES EN USOS DE SUELO


library(tidyverse)
library(terra)
library(readxl)
library(vegan)
library(landscapemetrics)
library(lme4)


source('scripts/0.1_pollen_stations.r')
source('scripts/0.1_pollen_stations.r')


# eliminamos estas categorias de usos de suelo porque menos del 60% de las celdas cambian para todos los cambios de periodo c('agricultural mosaic', 'broadleaved forests', 'coniferous forest', 'mixed forests', 'shrubland', 'tree crops')
# si las tenemos en cuenta con el tercer sistema de reclasificacion


# POLEN and CLIMATE DATA ####

pollen = read.csv("results/parametros.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, sm.ps) %>%
  subset(method=='percentage') %>%
  select(-method)
colnames(pollen)[colnames(pollen)=='seasons'] = 'año'

climate <- read.csv("results/climate_data_summary.txt", sep="")


# PREPARAMOS DF DE INCREMENTOS ####

# POLLEN CHANGES
pol_changes = expand_grid(site = unique(pollen$site),
                         año1 = c(2000, 2006, 2012, 2018),
                         año2 = c(2000, 2006, 2012, 2018),
                         type = unique(pollen$type))

# cuantificamos cambios de un periodo al siguiente
pol_changes = pol_changes[pol_changes$año2 > pol_changes$año1,]
pol_changes$Δ_años = pol_changes$año2 - pol_changes$año1
pol_changes <- pol_changes[pol_changes$Δ_años <= 6,]
pol_changes$Δ_años = NULL

# polen
pol_changes$Δ_polen = NA

# loop
for (i in 1:nrow(pol_changes)) {
  
  p1 = pollen %>%
    dplyr::filter(site == pol_changes$site[i],
                  type == pol_changes$type[i],
                  # hago la mediana de 3 años
                  año %in% (pol_changes$año1[i] + c(-1, 0, 1))) %>%
    dplyr::pull(sm.ps) %>% median(na.rm = TRUE)
  p2 = pollen %>%
    dplyr::filter(site == pol_changes$site[i],
                  type == pol_changes$type[i],
                  año %in% (pol_changes$año2[i] + c(-1, 0, 1))) %>%
    dplyr::pull(sm.ps) %>% median(na.rm = TRUE)
  
  pol_changes$Δ_polen[i] = p2 - p1

  # progress
  print(paste(round(i/nrow(pol_changes),4)*100, '%'))
  
}

# # save
# write.table(pol_changes, 'results/pol_changes.txt')



# LAND USE CHANGES
lu_changes = expand_grid(site = unique(pollen$site),
                         año1 = c(2000, 2006, 2012, 2018),
                         año2 = c(2000, 2006, 2012, 2018))

# cuantificamos cambios de un periodo al siguiente
lu_changes = lu_changes[lu_changes$año2 > lu_changes$año1,]
lu_changes$Δ_años = lu_changes$año2 - lu_changes$año1
lu_changes <- lu_changes[lu_changes$Δ_años <= 6,]
lu_changes$Δ_años = NULL


# CORINE reclassified 1: CORINErecl
# class metrics
lu_changes$Δ_urban = NA
lu_changes$Δ_nonirrigated_arable_land = NA
lu_changes$Δ_permanently_irrigated_land = NA
lu_changes$Δ_permanent_crops = NA
lu_changes$Δ_pastures = NA
lu_changes$Δ_agricultural_mosaic = NA
lu_changes$Δ_forests = NA
lu_changes$Δ_natural_grasslands = NA
lu_changes$Δ_shrubland = NA
lu_changes$Δ_sclerophyllous_forests = NA

# landscape metrics
lu_changes$Δ_ed = NA
lu_changes$Δ_pd = NA
lu_changes$Δ_shdi = NA

# loop
for (i in 1:nrow(lu_changes)) {

  # sitio + buffer 10km
  st = v_pollen_stations[v_pollen_stations$codigo==lu_changes$site[i],] %>%
    project(crs(CORINErecl)) %>% buffer(10000)
  
  # recorto CLC
  clcxst = CORINErecl %>% terra::crop(st, mask=T)

  # CORINErecl
  lu_changes$Δ_urban[i] = lsm_c_ca(clcxst[[grep(lu_changes$año2[i], names(clcxst))]])$value[1] - lsm_c_ca(clcxst[[grep(lu_changes$año1[i], names(clcxst))]])$value[1]
  lu_changes$Δ_nonirrigated_arable_land[i] = lsm_c_ca(clcxst[[grep(lu_changes$año2[i], names(clcxst))]])$value[2] - lsm_c_ca(clcxst[[grep(lu_changes$año1[i], names(clcxst))]])$value[2]
  lu_changes$Δ_permanently_irrigated_land[i] = lsm_c_ca(clcxst[[grep(lu_changes$año2[i], names(clcxst))]])$value[3] - lsm_c_ca(clcxst[[grep(lu_changes$año1[i], names(clcxst))]])$value[3]
  lu_changes$Δ_permanent_crops[i] = lsm_c_ca(clcxst[[grep(lu_changes$año2[i], names(clcxst))]])$value[4] - lsm_c_ca(clcxst[[grep(lu_changes$año1[i], names(clcxst))]])$value[4]
  lu_changes$Δ_pastures[i] = lsm_c_ca(clcxst[[grep(lu_changes$año2[i], names(clcxst))]])$value[5] - lsm_c_ca(clcxst[[grep(lu_changes$año1[i], names(clcxst))]])$value[5]
  lu_changes$Δ_agricultural_mosaic[i] = lsm_c_ca(clcxst[[grep(lu_changes$año2[i], names(clcxst))]])$value[6] - lsm_c_ca(clcxst[[grep(lu_changes$año1[i], names(clcxst))]])$value[6]
  lu_changes$Δ_forests[i] = lsm_c_ca(clcxst[[grep(lu_changes$año2[i], names(clcxst))]])$value[7] - lsm_c_ca(clcxst[[grep(lu_changes$año1[i], names(clcxst))]])$value[7]
  lu_changes$Δ_natural_grasslands[i] = lsm_c_ca(clcxst[[grep(lu_changes$año2[i], names(clcxst))]])$value[8] - lsm_c_ca(clcxst[[grep(lu_changes$año1[i], names(clcxst))]])$value[8]
  lu_changes$Δ_shrubland[i] = lsm_c_ca(clcxst[[grep(lu_changes$año2[i], names(clcxst))]])$value[9] - lsm_c_ca(clcxst[[grep(lu_changes$año1[i], names(clcxst))]])$value[9]
  lu_changes$Δ_sclerophyllous_forests[i] = lsm_c_ca(clcxst[[grep(lu_changes$año2[i], names(clcxst))]])$value[10] - lsm_c_ca(clcxst[[grep(lu_changes$año1[i], names(clcxst))]])$value[10]

  lu_changes$Δ_ed[i] = lsm_l_ed(clcxst[[grep(lu_changes$año2[i], names(clcxst))]])$value - lsm_l_ed(clcxst[[grep(lu_changes$año1[i], names(clcxst))]])$value
  lu_changes$Δ_pd[i] = lsm_l_pd(clcxst[[grep(lu_changes$año2[i], names(clcxst))]])$value - lsm_l_pd(clcxst[[grep(lu_changes$año1[i], names(clcxst))]])$value
  lu_changes$Δ_shdi[i] = lsm_l_shdi(clcxst[[grep(lu_changes$año2[i], names(clcxst))]])$value - lsm_l_shdi(clcxst[[grep(lu_changes$año1[i], names(clcxst))]])$value
  
  # progress
  print(paste(round(i/nrow(lu_changes),4)*100, '%'))
  
}

# # save
# write.table(lu_changes, 'results/lu_changes.txt')




# SELECCIONAMOS LAS VARIABLES ####

# juntamos los df de tendencias de polen y predictores
Δ_df = merge(pol_changes, lu_changes, by=c('site','año1','año2'))

# reordeno variables
Δ_df = Δ_df[,c("site", "type", "Δ_polen",
               # "Δ_temp_DJF", "Δ_temp_MAM", "Δ_temp_JJA", "Δ_temp_SON",
               "Δ_urban", "Δ_nonirrigated_arable_land", "Δ_permanently_irrigated_land", "Δ_permanent_crops", 
               "Δ_pastures", "Δ_agricultural_mosaic", "Δ_forests", "Δ_natural_grasslands", "Δ_shrubland",
               "Δ_sclerophyllous_forests",
               "Δ_ed", "Δ_pd", "Δ_shdi")]

# echamos un ojo 
pairs(log1p(abs(Δ_df[,4:16])), lower.panel=NULL)
corr = cor(Δ_df[,4:16], use='pairwise.complete.obs') %>% round(2)
corr[upper.tri(corr)] <- NA
round(corr, 2)



# MODELOS ####

# correlations
data_trends = Δ_df %>%
  pivot_longer(4:16, names_to='predictor', values_to='value') %>%
  group_by(type, predictor) %>%
  summarise({
    if (sum(!is.na(Δ_polen) & !is.na(value)) > 2) {
      test <- cor.test(Δ_polen, value, method="spearman")
      data.frame(
        rho = unname(test$estimate),
        p.val = test$p.value
      )
    } else {
      data.frame(rho = NA, p.val = NA)
    }
  }, .groups = "drop") %>%
  mutate(sig = ifelse(p.val < 0.05, "sig.", "non sig."),
         label = ifelse(sig == "sig.", "***", "")
  )

# 
data_trends$predictor = factor(data_trends$predictor, levels=c("Δ_agricultural_mosaic", "Δ_forests","Δ_natural_grasslands","Δ_nonirrigated_arable_land","Δ_pastures","Δ_permanent_crops","Δ_permanently_irrigated_land", "Δ_sclerophyllous_forests", "Δ_shrubland", "Δ_urban",      "Δ_ed", "Δ_pd", "Δ_shdi"))

ggplot(data_trends, aes(x = predictor, y = type, fill = rho)) +
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


