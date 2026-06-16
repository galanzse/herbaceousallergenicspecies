

# Exploramos tendencias en polen total y parametros


library(tidyverse)
library(AeRobiology)
# library(zoo)



# CORRELACIONES ####

# data
parametros = read.csv("results/parametros.txt", sep="")

# pairs(parametros) ~ metodos
parametros$method = as.factor(parametros$method)
colors <- as.numeric(parametros$method)
par(mar=c(5,5,2,8), oma = c(0,0,0,0), xpd=NA)
pairs(log(parametros[,5:10]), col=colors, pch=19, lower.panel=NULL)
legend("bottomleft", inset = c(0, 0),
       col=1:length(levels(parametros$method)), legend = levels(parametros$method),
       pch = 19, bty = "n")

# pairs(parametros) ~ especies
parametros$type = as.factor(parametros$type)
colors <- as.numeric(parametros$type)
par(mar=c(5,5,2,8), oma = c(0,0,0,0), xpd=NA)
pairs(log(parametros[,5:10]), col=colors, pch=19, lower.panel=NULL)
legend("bottomleft", inset = c(0, 0),
       col=1:length(levels(parametros$type)), legend = levels(parametros$type),
       pch = 19, bty = "n")


# continuamos con st.jd, ln.ps, sm.tt
parametros = read.csv("results/parametros.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, st.jd, ln.ps, sm.tt)



# SERIES TEMPORALES ####

# calculamos el polen total (integral) por estacion y type
annual_pollen_summary <- read.csv("results/parametros.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, st.jd, ln.ps, sm.tt) %>%
  pivot_longer(cols=c(st.jd, ln.ps, sm.tt), names_to='parameter', values_to='value')
colnames(annual_pollen_summary)[colnames(annual_pollen_summary)=='seasons'] = 'year'

# los diferentes metodos dan diferentes integrales pero tienen relaciones lineares perfectas
annual_pollen_summary %>%
  subset(parameter=='sm.tt') %>%
  pivot_wider(names_from=method, values_from=value) %>%
  dplyr::select(percentage, moving, grains, clinical, logistic) %>%
  pairs()

# APIn de URTI y PLAN muestran tendencias al alza desde 2010
annual_pollen_summary$parameter = factor(annual_pollen_summary$parameter, levels=c("st.jd","ln.ps","sm.tt"))
annual_pollen_summary = annual_pollen_summary %>% na.omit() %>% subset(value<8000)
ggplot(aes(x=year, y=value, group=site, color=site),
       data=annual_pollen_summary[annual_pollen_summary$method=='percentage',]) +
  geom_point(size=0.5, pch=5) +
  geom_smooth(method='loess', span=1, se=F) +
  labs(title = "Polen total anual 1994-2025", x="año", y="log polen total anual") +
  scale_x_continuous(limits=c(1994, 2026), expand = c(0, 0)) +
  # scale_y_continuous(limits=c(0, 6000), expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position='bottom') +
  guides(colour=guide_legend(nrow=2, byrow = TRUE)) +
  facet_grid(parameter~type, scales='free_y')



# IMPORTANCIA DEL METODO ####

# comparacion entre metodos para st.jd, ln.ps, sm.tt
temp <- parametros %>% dplyr::select(type, site, method, seasons, st.jd) %>%
  pivot_wider(names_from=method, values_from=st.jd)
temp$type = as.factor(temp$type)
colors <- as.numeric(temp$type)
par(mar=c(4,4,4,4), oma = c(0,0,0,5), xpd=NA)
pairs(temp[,c('percentage', 'logistic', 'moving', 'grains', 'clinical')],
      col=colors, pch=19, lower.panel=NULL)
legend("bottomleft",
       inset = 0,
       legend = levels(temp$type), col = 1:length(levels(temp$type)),
       pch = 19, bty = "n", pt.cex = 1, cex = 1, y.intersp = 1)


# long
parametros_long <- parametros %>% pivot_longer(5:7, names_to='fenofase', values_to='value')
parametros_long$fenofase <- factor(parametros_long$fenofase, levels=c("st.jd", "ln.ps", "sm.tt"))

# diferencias entre tipos y estaciones
ggplot(aes(x=type, y=value, fill=type), data=parametros_long) +
  geom_boxplot() +
  labs(title = "Comparacion entre tipos", x=NULL, y="valor") +
  facet_wrap(~fenofase+method, nrow=3, scales='free_y') +
  theme_bw() +
  theme(axis.text.x=element_blank(), legend.position='bottom')

ggplot(aes(x=method, y=value, fill=method), data=parametros_long) +
  geom_boxplot() +
  labs(title = "Comparacion ente metodos", x=NULL, y="valor") +
  facet_wrap(~fenofase+type, nrow=3, scales='free_y') +
  theme_bw() +
  theme(axis.text.x=element_blank(), legend.position='bottom')



# PARAMETROS POR ESTACION Y TIPO POLINICO ####

parametros = read.csv("results/parametros.txt", sep="") %>%
  dplyr::select(-en.jd, -sm.ps, pk.val, -pk.jd, -daysth, -pk.val)
colnames(parametros)[colnames(parametros)=='seasons'] = 'year'

annual_pollen_summary <- parametros %>%
  group_by(site, type, year) %>%
  summarise(
    st.jd = median(st.jd, na.rm=T),
    ln.ps = median(ln.ps, na.rm=T),
    sm.tt = median(sm.tt, na.rm=T)
  ) %>%
  pivot_longer(cols=c(st.jd, ln.ps, sm.tt), names_to='parameter')

annual_pollen_summary = annual_pollen_summary %>% na.omit() %>% subset(value<3000)

annual_pollen_summary$parameter = factor(annual_pollen_summary$parameter, levels=c("st.jd","ln.ps","sm.tt"))

# exploramos diferencias entre estaciones 1
ggplot(aes(x=type, y=value, fill=type),
       data=annual_pollen_summary) +
  geom_boxplot() +
  # geom_point() +
  labs(x=NULL, y="APIn") +
  # scale_y_continuous(limits=c(0, 3000), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x=element_blank(), legend.position='bottom') +
  guides(fill=guide_legend(nrow=1, byrow = TRUE)) +
  facet_grid(parameter ~ site, scales='free_y')

# exploramos diferencias entre estaciones 2
ggplot(aes(x=site, y=value, fill=site), data=annual_pollen_summary) +
  geom_boxplot() +
  labs(x=NULL, y="value") +
  theme_bw() +
  theme(axis.text.x=element_blank(), legend.position='bottom') +
  guides(fill=guide_legend(nrow=1, byrow = TRUE)) +
  facet_grid(parameter ~ type, scales='free_y')


