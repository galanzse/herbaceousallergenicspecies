

# CORINE LAND COVER (2017-2018)


# Saco un figura como la Fig. 4 Picornell et al 2023: diferencias entre estaciones y cambios temporales

# Fig. 4 Picornell et al 2023: Spearman correlation and significance of the index calculated for each land use type within a radius of 5, 10, 22 km


library(tidyverse)
library(terra)
library(readxl)
library(vegan)

source('scripts/0.1_estaciones.r')



# CORINE accounting layers ####

t0 = '/Users/jesusrojo/Library/CloudStorage/OneDrive-Personal/ACADEMICO/proyectos/fuentes_palinocam/data/CORINE_acc/'
CORINEacc = c(rast(paste0(t0, 'eea_r_3035_100_m_clc-1990-acc_p_1986-1998_v18_r05/clc1990_acc_V18_5.tif')),
              rast(paste0(t0, 'eea_r_3035_100_m_clc-2000-acc_p_1999-2001_v02_r00/CLC2000ACC_V2018_20.tif')),
              rast(paste0(t0, 'eea_r_3035_100_m_clc-2006-acc_p_2005-2007_v02_r00/CLC2006ACC_V2018_20.tif')),
              rast(paste0(t0, 'eea_r_3035_100_m_clc-2012-acc_p_2011-2013_v02_r00/CLC2012ACC_V2018_20.tif')),
              rast(paste0(t0, 'eea_r_3035_100_m_clc-2018-acc_p_2017-2018_v01_r00/CLC2018ACC_V2018_20.tif')))
# names
names(CORINEacc) = c('clc1990', 'clc2000', 'clc2006', 'clc2012', 'clc2018')

# roi
roi = terra::buffer(v_pollen_stations, width=22000)[,'codigo'] %>% aggregate() %>% project(CORINEacc)
CORINEacc = crop(CORINEacc, roi, mask=T)


# reclassify CORINEacc$clc1990
CORINErecl <- CORINEacc
CORINErecl <- as.numeric(CORINErecl)

# reclassify the others
reclass_df <- read_excel("data/CORINEacc_reclassification.xlsx")
from = reclass_df$CLC_CODE
to = reclass_df$DESCRIPCION_rcl
for (i in 1:nlyr(CORINErecl)) { CORINErecl[[i]] <- subst(CORINErecl[[i]], from = from, to = to) }
names(CORINErecl) = c('clc1990', 'clc2000', 'clc2006', 'clc2012', 'clc2018')

par(mfrow=c(2,3))
pts1 = project(v_pollen_stations,CORINErecl)
plot(CORINErecl$clc1990, legend=NULL, main='Corine 1990'); points(pts1, col='red', cex=2)
plot(CORINErecl$clc2000, legend=NULL, main='Corine 2000'); points(pts1, col='red', cex=2)
plot(CORINErecl$clc2006, legend=NULL, main='Corine 2006'); points(pts1, col='red', cex=2)
plot(CORINErecl$clc2012, legend=NULL, main='Corine 2012'); points(pts1, col='red', cex=2)
plot(CORINErecl$clc2018, main='Corine 2018'); points(pts1, col='red', cex=2)



# EXTRAER DATOS PARA CADA ESTACION ####

# buffer de 10 km alrededor de cada estacion
buf = terra::buffer(v_pollen_stations, width=5000)[,'codigo'] %>% project(CORINErecl)

# extraigo el numero de pixel por año y categoria
years = as.numeric(str_sub(names(CORINErecl), -4))
CORINE_counts <- lapply(seq_along(years), function(i) {
  
  r <- CORINErecl[[i]]
  
  out <- extract(r, buf, fun=table, na.rm=TRUE)
  
  out$site <- buf$codigo
  out$año <- years[i]
  
  return(out)
})

# lo uno
CORINE_counts <- do.call(rbind, CORINE_counts)
CORINE_counts$ID = NULL

# formato largo
CORINE_counts <- CORINE_counts %>% pivot_longer(1:12, names_to='DESCRIPCION_rcl', values_to='counts')
CORINE_counts$area_km2 = CORINE_counts$counts * 0.01
CORINE_counts$counts = NULL


# DIFERENCIAS ESPACIALES EN USOS DE SUELO ####

# analisis NMDs
CORINE_nmds = CORINE_counts %>%
  mutate(DESCRIPCION_rcl=gsub(" ", "_", DESCRIPCION_rcl)) %>%
  pivot_wider(names_from='DESCRIPCION_rcl', values_from='area_km2') %>%
  mutate(sitexyear=paste0(site, '_', año)) %>%
  select(-año, -site) %>%
  as.data.frame()
rownames(CORINE_nmds) = CORINE_nmds$sitexyear
CORINE_nmds$sitexyear = NULL

nmds <- metaMDS(CORINE_nmds, distance="bray", k=2, trymax=100)
# saveRDS(nmds, 'results/nmds.rds')



# DIFERENCIAS ESPACIALES: QUE USOS DE SUELO SE CORRELACIONAN CON EL POLEN
pollen = read.csv("results/fenofases.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, sm.ps) %>%
  subset(method=='percentage') %>%
  mutate(method=NULL) %>%
  group_by(type, site) %>%
  summarise(APIn_2000 = mean(sm.ps[seasons %in% 1999:2001], na.rm=TRUE),
            APIn_2006 = mean(sm.ps[seasons %in% 2005:2007], na.rm=TRUE),
            APIn_2012 = mean(sm.ps[seasons %in% 2011:2013], na.rm=TRUE),
            APIn_2018 = mean(sm.ps[seasons %in% 2017:2019], na.rm=TRUE),
            .groups = "drop") %>%
  pivot_longer(cols=c(APIn_2000, APIn_2006, APIn_2012, APIn_2018), names_to='año', values_to='APIn')

pollen$año[pollen$año=='APIn_2000'] = 2000
pollen$año[pollen$año=='APIn_2006'] = 2006
pollen$año[pollen$año=='APIn_2012'] = 2012
pollen$año[pollen$año=='APIn_2018'] = 2018
pollen$año = as.numeric(pollen$año)

# unimos
CORINE_x_Polen = merge(CORINE_counts, pollen, by=c('site','año'))



# modelos lineales
CORINE_x_Polen$año = as.factor(CORINE_x_Polen$año)
ggplot(aes(x=area_km2, y=APIn, group=año, color=site, shape=año),
       data=CORINE_x_Polen[CORINE_x_Polen$type=='URTI',]) +
  geom_point() +
  geom_smooth(method='lm', se=F) +
  ggtitle('AMAR') +
  facet_wrap(~DESCRIPCION_rcl, scales='free_x')

CORINE_x_Polen$año = as.factor(CORINE_x_Polen$año)
ggplot(aes(x=area_km2, y=APIn, group=año, color=site, shape=año),
       data=CORINE_x_Polen[CORINE_x_Polen$type=='URTI',]) +
  geom_point() +
  geom_smooth(method='lm', se=F) +
  ggtitle('AMAR') +
  facet_wrap(~DESCRIPCION_rcl, scales='free_x')


# hacemos correlaciones
cor_CxP = CORINE_x_Polen %>% group_by(DESCRIPCION_rcl, type, año) %>%
  summarise(rho = cor.test(APIn, area_km2, method='spearman')$estimate,
            pval = cor.test(APIn, area_km2, method='spearman')$p.value)

cor_CxP$sig <- ifelse(cor_CxP$pval < 0.05, "sig.", "non sig.")

df <- cor_CxP %>%
  mutate(
    label = ifelse(sig == "sig.", "***", ""),
    rho = as.numeric(rho)
  )

ggplot(df, aes(x = DESCRIPCION_rcl, y = type, fill = rho)) +
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
  labs(fill = "rho", x = "Site", y = "Type")

# amar destaca en zonas cultivadas
# arte, plan y rumi muestras patrones dificiles de interpretar
# rume destaca en bosques y zonas agroforestales



# DIFERENCIAS TEMPORALES: QUE CAMBIOS EN USOS EXPLICAN CAMBIOS EN POLEN ####

# polen
pollen = read.csv("results/fenofases.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, sm.ps) %>%
  subset(method=='percentage') %>%
  select(-method)
colnames(pollen)[colnames(pollen)=='seasons'] = 'año'

# interpolo la serie
int_CORINE_counts <- CORINE_counts %>%
  group_by(site, DESCRIPCION_rcl) %>%
  complete(año = seq(min(año), max(año), by = 1)) %>%
  arrange(año) %>%
  mutate(
    area_interp = approx(
      x = año[!is.na(area_km2)],
      y = area_km2[!is.na(area_km2)],
      xout = año,
      method = "linear",
      rule = 2
    )$y
  ) %>%
  select(-area_km2) %>%
  ungroup()

# unimos
int_CORINE_x_Polen = merge(int_CORINE_counts, pollen, by=c('site','año'))

# # eg
# View(int_CORINE_x_Polen[int_CORINE_x_Polen$site=='ALCA' & int_CORINE_x_Polen$DESCRIPCION_rcl=='urban' & int_CORINE_x_Polen$type=='AMAR',])

# hacemos correlaciones
cor_CxP = int_CORINE_x_Polen %>% group_by(DESCRIPCION_rcl, type, site) %>%
  summarise(rho = cor.test(sm.ps, area_interp, method='spearman')$estimate,
            pval = cor.test(sm.ps, area_interp, method='spearman')$p.value)

cor_CxP$sig <- ifelse(cor_CxP$pval < 0.05, "sig.", "non sig.")

# heatmap
df <- cor_CxP %>%
  mutate(
    label = ifelse(sig == "sig.", "***", ""),
    rho = as.numeric(rho)
  )

ggplot(df, aes(x = site, y = type, fill = rho)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label), size = 3) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    na.value = "grey90"
  ) +
  facet_wrap(~DESCRIPCION_rcl) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  labs(fill = "rho", x = "Site", y = "Type")


