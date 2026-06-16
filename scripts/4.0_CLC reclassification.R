
# CORINE RECLASSIFICATION

library(tidyverse)
library(terra)
library(readxl)

# t0 = '/Users/jesusrojo/Library/CloudStorage/OneDrive-Personal/ACADEMICO/proyectos/fuentes_palinocam/data/CORINE_acc/'
t0 = "C:/Users/javie/OneDrive/ACADEMICO/proyectos/fuentes_palinocam/data/CORINE_acc/"
CORINEacc = c(rast(paste0(t0, 'eea_r_3035_100_m_clc-2000-acc_p_1999-2001_v02_r00/CLC2000ACC_V2018_20.tif')),
              rast(paste0(t0, 'eea_r_3035_100_m_clc-2006-acc_p_2005-2007_v02_r00/CLC2006ACC_V2018_20.tif')),
              rast(paste0(t0, 'eea_r_3035_100_m_clc-2012-acc_p_2011-2013_v02_r00/CLC2012ACC_V2018_20.tif')),
              rast(paste0(t0, 'eea_r_3035_100_m_clc-2018-acc_p_2017-2018_v01_r00/CLC2018ACC_V2018_20.tif')))
# names
names(CORINEacc) = c('clc2000', 'clc2006', 'clc2012', 'clc2018')

# roi
roi = terra::buffer(v_pollen_stations, width=22000)[,'codigo'] %>% aggregate() %>% project(CORINEacc)
CORINEacc = crop(CORINEacc, roi, mask=T)


# reclassify
reclass_df <- read_excel("data/CORINEacc_reclassification.xlsx")

# CLC reclasificado en categorias para las que hay cambios
CORINErecl <- CORINEacc
CORINErecl <- as.numeric(CORINErecl)
from = reclass_df$CLC_CODE
to = reclass_df$RCL_CODE
for (i in 1:nlyr(CORINErecl)) { CORINErecl[[i]] <- subst(CORINErecl[[i]], from = from, to = to) }

# assign levels
cats <- data.frame(value = reclass_df$RCL_CODE, class = reclass_df$RCL_NAME) %>% na.omit() %>% unique()
levels(CORINErecl)[[1]] <- cats
levels(CORINErecl)[[2]] <- cats
levels(CORINErecl)[[3]] <- cats
levels(CORINErecl)[[4]] <- cats

names(CORINErecl) = c('clc2000', 'clc2006', 'clc2012', 'clc2018')

par(mfrow=c(2,2))
pts1 = project(v_pollen_stations, CORINErecl)
plot(CORINErecl$clc2000, legend=NULL, main='Corine 2000'); points(pts1, col='red', cex=1.5)
plot(CORINErecl$clc2006, legend=NULL, main='Corine 2006'); points(pts1, col='red', cex=1.5)
plot(CORINErecl$clc2012, legend=NULL, main='Corine 2012'); points(pts1, col='red', cex=1.5)
plot(CORINErecl$clc2018, main='Corine 2018'); points(pts1, col='red', cex=1.5)


