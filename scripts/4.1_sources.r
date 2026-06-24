

# CORINE LAND COVER (2017-2018)

library(tidyverse)
library(terra)
library(readxl)
library(vegan)
library(landscapemetrics)

source('scripts/0.1_pollen_stations.r')
source('scripts/4.0_CLC reclassification.r')



# POLLEN DATA ####

pollen = read.csv("results/parametros.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, sm.tt) %>%
  subset(method=='percentage') %>%
  unique() %>%
  mutate(method=NULL) %>%
  group_by(type, site) %>%
  summarise(sm.tt_2000 = mean(sm.tt[seasons %in% 1999:2001], na.rm=TRUE),
            sm.tt_2006 = mean(sm.tt[seasons %in% 2005:2007], na.rm=TRUE),
            sm.tt_2012 = mean(sm.tt[seasons %in% 2011:2013], na.rm=TRUE),
            sm.tt_2018 = mean(sm.tt[seasons %in% 2017:2019], na.rm=TRUE),
            .groups = "drop") %>%
  pivot_longer(cols=c(sm.tt_2000, sm.tt_2006, sm.tt_2012, sm.tt_2018), names_to='año', values_to='sm.tt')

colnames(pollen)[colnames(pollen)=='sm.tt'] = 'APIn'

pollen$año[pollen$año=='sm.tt_2000'] = 2000
pollen$año[pollen$año=='sm.tt_2006'] = 2006
pollen$año[pollen$año=='sm.tt_2012'] = 2012
pollen$año[pollen$año=='sm.tt_2018'] = 2018
pollen$año = as.numeric(pollen$año)



# EXTRAER DATOS PARA CADA ESTACION ####

vars <- c("urban", "nonirrigated_arable_land", "permanently_irrigated_land", "permanent_crops", "pastures", "agricultural_mosaic",   "forests", "natural_grasslands", "shrubland", "sclerophyllous_forests", "ed", "pd", "shdi")

sources <- expand_grid(
  site = unique(pollen$site),
  año = c(2000, 2006, 2012, 2018)) %>% as.data.frame()

sources[vars] <- NA_real_

for (i in seq_len(nrow(sources))) {
  
  # Site + 10-km buffer
  st <- v_pollen_stations[v_pollen_stations$codigo == sources$site[i], ] %>%
    project(crs(CORINErecl)) %>% buffer(10000)
  
  # Crop and select year
  clcxst <- terra::crop(CORINErecl, st, mask = TRUE)
  clcxst <- clcxst[[grep(sources$año[i], names(clcxst))]]
  
  # Land-cover proportions
  lvl1 <- levels(clcxst)[[1]]
  colnames(lvl1) <- c("class", "name")
  lvl1 <- merge(lvl1, lsm_c_ca(clcxst)[, c("class", "value")], by = "class", all.x = TRUE)
  lvl1$value[is.na(lvl1$value)] <- 0
  
  # Store class metrics
  vars <- c("urban","nonirrigated arable land","permanently irrigated land","permanent crops","pastures",
            "agricultural mosaic","forests","natural grasslands","shrubland","sclerophyllous forests")
  
  cols <- c("urban","nonirrigated_arable_land","permanently_irrigated_land","permanent_crops","pastures",
            "agricultural_mosaic","forests","natural_grasslands","shrubland","sclerophyllous_forests")
  
  for (j in seq_along(vars)) {
    sources[i, cols[j]] <- lvl1$value[match(vars[j], lvl1$name)]
    }
  
  # Landscape metrics
  sources$ed[i]   <- lsm_l_ed(clcxst)$value
  sources$pd[i]   <- lsm_l_pd(clcxst)$value
  sources$shdi[i] <- lsm_l_shdi(clcxst)$value
  
  # Progress
  cat(sprintf("%.1f%%\n", 100 * i / nrow(sources)))
  
}



# DIFERENCIAS ESPACIALES EN USOS DE SUELO ####

pollen$sitexyear = paste0(pollen$site, '_', pollen$año)
pollen = pollen %>% select(-site, -año) %>%
  pivot_wider(names_from = type, values_from = APIn) %>%
  na.omit() %>% as.data.frame()
rownames(pollen) = pollen$sitexyear; pollen$sitexyear = NULL

nmds <- metaMDS(pollen,  distance = "bray",  k = 2,  trymax = 100)

# stress
nmds$stress

# APIn matrix
rownames(sources) = paste0(sources$site, '_', sources$año)
sources_mat <- sources %>% select(-site, -año, -ed, -pd, -shdi)
sources_mat <- sources_mat[rownames(pollen), ]

# Fit pollen vectors
fit_pollen <- envfit(nmds,  sources_mat,  permutations = 999,  na.rm = TRUE)
print(fit_pollen)

# Plot
plot(nmds,  type = "n")

points(nmds,  display = "species",  pch = 19)
text(nmds,  display = "species",  cex = 0.6,  pos = 2, col='blue')

orditorp(nmds, display="sites", cex=0.7, col="black")

# significant pollen vectors (red)
plot(fit_pollen,  p.max = 0.05,  cex = 0.8, col = "red", font=2)


