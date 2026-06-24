

# Calculamos las parametros por tipo polinico y año


library(tidyverse)
library(AeRobiology)
library(zoo)


# datos de polen
pollen <- read.csv("results/curated_pollen_data.txt", sep="") %>%
  dplyr::select(site, date, type, gr_x_m3) %>%
  pivot_wider(names_from=type, values_from=gr_x_m3)

# formateamos bien las fechas
pollen$date <- as.Date(pollen$date)

# convertimos en lista
pollen_wide <- split(pollen, pollen$site)

# eliminamos columna de sitio
pollen_wide <- lapply(pollen_wide, function(df) df[,-which(colnames(df)=='site')])



# CALCULO DE PARAMETROS

# tabla de combinaciones y lista de resultados
combn <- expand_grid(site=unique(pollen$site),
                     type=c("AMAR", "ARTE", "PLAN", "RUME", "URTI"),
                     method=c("percentage", "logistic", "moving", 'grains', 'clinical'))

# el modelo logistico lo dejamos para curvas con un pico y datos suficientes
combn <- combn[!(combn$method == "logistic" & combn$type %in% c("AMAR", "ARTE", "URTI")), ]

# restringimos parametros a meses normales
floracion <- list(AMAR=c(3:11), ARTE=c(7:12), PLAN=c(3:8), RUME=c(3:6), URTI=c(2:8))

# resultados
parametros <- list()

# calculamos parametros
for (i in 1:nrow(combn)) {
  
  # selecciono sitio
  st = deframe(combn[i,'site'])
  temp = pollen_wide[st] %>% as.data.frame()
  colnames(temp) = c("date", "AMAR", "ARTE", "PLAN", "RUME", "URTI")
  
  # selecciono tipo polinico
  tp = deframe(combn[i,'type'])
  temp = temp %>% dplyr::select(date,all_of(tp))
  
  # restringimos los calculos a la ventana de floracion normal
  temp[!(month(temp$date) %in% deframe(floracion[tp])),2] <- 0
  
  # calculo parametros utilizando 5 metodos
  mt = deframe(combn[i,'method'])
  
  # media movil para facilitar logistico
  if (mt=='logistic') { temp[,2] = rollmean(temp[,2], k=ifelse(tp=='ARTE', 5, 11), fill=NA) }
  
  # calculamos las parametros
  temp = calculate_ps(temp,
                      # metodo
                      method=mt,
                      # percentage
                      perc=90,
                      # moving
                      man=11,
                      th.ma=3,
                      # logistic
                      derivative=5, reduction=TRUE, red.level=0.90,
                      # grains
                      window.grains=5,
                      th.pollen=3,
                      # clinical
                      n.clinical = 5,
                      window.clinical = 7,
                      th.sum = ifelse(tp=='ARTE', 10, 30),
                      type = "none",
                      # other arguments
                      def.season="natural", th.day=ifelse(tp=='ARTE', 7, 25),
                      interpolation=FALSE, export.plot=F)
  
  # añadimos info
  temp$site <- st
  temp$method <- mt

  # filtramos variables utiles y guardamos
  parametros[[i]] <- temp %>% dplyr::select(type, site, method, seasons, st.jd, en.jd, ln.ps, sm.tt, sm.ps, pk.val, pk.jd, daysth)

}

# colapso la lista
parametros = do.call(rbind, parametros) %>% as.data.frame()
# # para ARTE nos quedamos con polen total y picos
# parametros[parametros$type=='ARTE', which(colnames(parametros)%in%c('st.jd', 'en.jd', 'ln.ps'))] <- NA
# # guardo
# write.table(parametros, file = "results/parametros.txt")



# modelos
mod_data = read.csv("results/parametros.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, st.jd, ln.ps, sm.tt) %>%
  subset(type!='ARTE' & method!='logistic')

mod_data$type = as.factor(mod_data$type)
mod_data$site = as.factor(mod_data$site)
mod_data$method = as.factor(mod_data$method)
mod_data$seasons <- as.numeric(mod_data$seasons)
mod_data$st.jd = as.numeric(mod_data$st.jd)
mod_data$ln.ps = as.numeric(mod_data$ln.ps)
mod_data$sm.tt = as.numeric(mod_data$sm.tt)

str(mod_data)

mod <- lm(st.jd ~ type + site + method + seasons, data=mod_data)
Anova(mod, type="II") 
t = Anova(mod, type="II"); round(t$`Sum Sq`/sum(t$`Sum Sq`), 2)

mod <- lm(ln.ps ~ type + site + method + seasons, data=mod_data)
Anova(mod, type="II") 
t = Anova(mod, type="II"); round(t$`Sum Sq`/sum(t$`Sum Sq`), 2)

mod <- lm(sm.tt ~ type + site + method + seasons, data=mod_data)
Anova(mod, type="II") 
t = Anova(mod, type="II"); round(t$`Sum Sq`/sum(t$`Sum Sq`), 2)


