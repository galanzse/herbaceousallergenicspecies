

# ASOCIAR INCREMENTOS DE POLEN A VARIACIONES EN USOS DE SUELO


library(tidyverse)
library(terra)
library(readxl)
library(vegan)
library(landscapemetrics)
# library(lme4)


source('scripts/0.1_pollen_stations.r')
source('scripts/4.0_CLC reclassification.r')



# POLLEN DATA ####

pollen = read.csv("results/parametros.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, sm.tt) %>%
  subset(method=='percentage') %>%
  select(-method)
colnames(pollen)[colnames(pollen)=='seasons'] = 'year'

ventanas <- tibble(
  year = c(2000, 2006, 2012, 2018)) %>%
  mutate(years_mean = purrr::map(year, ~ .x + c(-1, 0, 1)))

pollen_clc <- map_dfr(seq_len(nrow(ventanas)), function(i){
  yr <- ventanas$year[i]
  pollen %>%
    filter(year %in% ventanas$years_mean[[i]]) %>%
    group_by(site, type) %>%
    summarise(
      av_APIn = mean(sm.tt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(year = yr)
})



# LAND USES ####

# class metrics
pollen_clc$urban = NA
pollen_clc$nonirrigated_arable_land = NA
pollen_clc$permanently_irrigated_land = NA
pollen_clc$permanent_crops = NA
pollen_clc$pastures = NA
pollen_clc$agricultural_mosaic = NA
pollen_clc$forests = NA
pollen_clc$natural_grasslands = NA
pollen_clc$shrubland = NA
pollen_clc$sclerophyllous_forests = NA

# Landscape metrics
pollen_clc$ed <- NA
pollen_clc$pd <- NA
pollen_clc$shdi <- NA

# loop
for (i in 1:nrow(pollen_clc)) {
  
  # sitio + buffer 10km
  st = v_pollen_stations[v_pollen_stations$codigo==pollen_clc$site[i],] %>%
    project(crs(CORINErecl)) %>% buffer(10000)
  
  # recorto CLC
  clcxst = CORINErecl %>% terra::crop(st, mask=T)
  clcxst = clcxst[[grep(pollen_clc$year[i], names(clcxst))]]
  
  # areas
  classes = levels(clcxst)[[1]]; colnames(classes) = c('class', 'name')
  areas = merge(lsm_c_ca(clcxst)[,c('class','value')], classes, all=T)
  
  # CORINErecl
  pollen_clc$urban[i] = areas$value[areas$name=='urban']
  pollen_clc$nonirrigated_arable_land[i] = areas$value[areas$name=='nonirrigated arable land']
  pollen_clc$permanently_irrigated_land[i] =areas$value[areas$name=='permanently irrigated land']
  pollen_clc$permanent_crops[i] = areas$value[areas$name=='permanent crops']
  pollen_clc$pastures[i] = areas$value[areas$name=='pastures']
  pollen_clc$agricultural_mosaic[i] = areas$value[areas$name=='agricultural mosaic']
  pollen_clc$forests[i] = areas$value[areas$name=='forests']
  pollen_clc$natural_grasslands[i] = areas$value[areas$name=='natural grasslands']
  pollen_clc$shrubland[i] = areas$value[areas$name=='shrubland']
  pollen_clc$sclerophyllous_forests[i] = areas$value[areas$name=='sclerophyllous forests']

  # landscape
  pollen_clc$ed[i]   <- lsm_l_ed(clcxst)$value
  pollen_clc$pd[i]   <- lsm_l_pd(clcxst)$value
  pollen_clc$shdi[i] <- lsm_l_shdi(clcxst)$value
  
  # progress
  print(paste(round(i/nrow(pollen_clc),4)*100, '%'))
  
}

# NAs to 0
pollen_clc[,5:14][is.na(pollen_clc[,5:14])] = 0

# number of 0s per variable
colSums(pollen_clc[, 5:14] == 0, na.rm = TRUE)/nrow(pollen_clc)*100


# SELECCIONAMOS LAS VARIABLES ####

land_vars <- c("urban", "nonirrigated_arable_land", "permanently_irrigated_land", "permanent_crops", "pastures",         "agricultural_mosaic", "forests", "natural_grasslands", "shrubland", "sclerophyllous_forests", "ed", "pd", "shdi")

pollen_change <- pollen_clc %>%
  arrange(site, type, year) %>%
  group_by(site, type) %>%
  mutate(
    period = paste(lag(year), year, sep = "-"),
    d_polen = (av_APIn - lag(av_APIn))
  ) %>%
  mutate(
    across(
      all_of(land_vars),
      ~ (.x - lag(.x)), .names = "{.col}"
    )
  ) %>%
  filter(!is.na(period))

# retenemos cambios relevantes
pollen_change = pollen_change[-which(pollen_change$year==2000),-which(colnames(pollen_change)%in%c("av_APIn","year"))]

# order
pollen_change = pollen_change[,c("site", "type", "period", "d_polen", "shrubland", "sclerophyllous_forests", "forests",  "natural_grasslands", "pastures", "urban", "agricultural_mosaic", "nonirrigated_arable_land", "permanently_irrigated_land", "permanent_crops", "ed", "pd", "shdi")]

# CV
sapply(pollen_change[, 4:17], function(x) mean(abs(x), na.rm = TRUE))

# heat map
pollen_change_long = pollen_change %>% pivot_longer(5:17, names_to='class', values_to = 'diff_area')

pollen_change_cor = pollen_change_long %>%
  group_by(type, class) %>%
  summarise(
    rho = unname(cor.test(d_polen, diff_area, method="spearman")$estimate),
    p.val = cor.test(d_polen, diff_area, method="spearman")$p.value) %>%
  mutate(sig = ifelse(p.val < 0.05, "sig.", "non sig."),
         label = ifelse(sig == "sig.", "***", "")
  )

pollen_change_cor$type = factor(pollen_change_cor$type, levels=rev(c("AMAR", "ARTE", "PLAN", "RUME", "URTI")))
pollen_change_cor$class = factor(pollen_change_cor$class, levels=c("shrubland", "sclerophyllous_forests", "forests",  "natural_grasslands", "pastures", "urban", "agricultural_mosaic", "nonirrigated_arable_land", "permanently_irrigated_land", "permanent_crops", "ed", "pd", "shdi"))

ggplot(pollen_change_cor, aes(x = class, y = type, fill = rho)) +
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
  labs(fill = "rho", x = NULL, y = "Pollen type")


