
# Appendix S6

library(tidyverse)

# datos de clima
climate <- read.csv("results/clima_data_summary.txt", sep="") %>%
  mutate(year = season_year) %>% 
  dplyr::select(site, year, season, tmax_m, tmin_m) %>%
  pivot_longer(cols=c(tmax_m, tmin_m),
               names_to='meteo_sum', values_to='meteoval')
str(climate)
climate$site <- as.factor(climate$site)
climate$season <- as.factor(climate$season)
climate$meteoval[climate$meteoval==0] <- NA


# datos de polen
parametros <- read.csv("results/parametros.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, st.jd, ln.ps, sm.tt)
colnames(parametros)[colnames(parametros)=='seasons'] = 'year'

# unimos
climatexpollen <- merge(climate, parametros, by=c('site','year'))

# eliminamos temperaturas acumuladas
climatexpollen <- climatexpollen[climatexpollen$meteo_sum %in% c('tmax_m','tmin_m'),]


# calculamos tendencias
climate_trends = climatexpollen %>%
  pivot_longer(cols=c(st.jd, ln.ps, sm.tt), names_to='fenofase', values_to='fenoval') %>%
  group_by(type, site, method, fenofase, season, meteo_sum) %>%
  summarise({
    if (sum(!is.na(meteoval) & !is.na(fenoval)) > 2) {
      test <- cor.test(meteoval, fenoval, method = "spearman")
      data.frame(
        rho = unname(test$estimate),
        p.val = test$p.value
      )
    } else {
      data.frame(rho = NA, p.val = NA)
    }
  }, .groups = "drop") %>%
  ungroup()

# añador significacion
climate_trends$sig <- ifelse(climate_trends$p.val < 0.05, "sig.", "non sig.")

# ordenamos niveles
climate_trends$season <- factor(climate_trends$season, levels=c('DJF', 'MAM', 'JJA', 'SON'))
climate_trends$fenofase <- factor(climate_trends$fenofase, levels=c('st.jd', 'ln.ps', 'sm.tt'))
levels(climate_trends$fenofase) = c('SOP', 'LOP', 'APIn')

# eliminamos ARTE
climate_trends <- climate_trends[climate_trends$type!='ARTE',]

# para APIn nos quedamos con un metodo
climate_trends$method[climate_trends$fenofase=='APIn'] = 'clinical'
climate_trends = unique(climate_trends)

# eliminamos comparaciones
climate_trends$rho[climate_trends$season=='SON' & climate_trends$fenofase=='SOP'] <- NA
climate_trends$rho[climate_trends$season=='JJA' & climate_trends$fenofase=='SOP'] <- NA
climate_trends = climate_trends %>% subset(climate_trends$season!='SON')

# tabla
temp = climate_trends[climate_trends$season=='DJF' & climate_trends$type=='URTI',]
table(temp$sig, temp$fenofase, temp$meteo_sum)

climate_trends$meteo_sum <- factor(
  climate_trends$meteo_sum,
  levels = c("tmax_m", "tmin_m"),
  labels = c("Max_temp", "Min_temp")
)

colnames(climate_trends)[colnames(climate_trends)%in%'meteo_sum'] = 'Meteo'
colnames(climate_trends)[colnames(climate_trends)%in%'sig'] = 'Sig.'

ggplot(climate_trends,
       aes(x = fenofase,
           y = rho,
           fill = Meteo)) +
  geom_point(
    aes(colour = Sig.,
        group = Meteo),
    position = position_jitterdodge(
      jitter.width = 0.2,
      dodge.width = 0.8
    ),
    alpha = 0.7,
    size = 1.2
  ) +
  
  geom_boxplot(
    position = position_dodge(width = 0.8),
    outlier.shape = NA
  ) +
  
  geom_hline(yintercept = 0) +
  
  facet_grid(season ~ type, scales = "free_x") +
  
  labs(y="Spearman's p", x=NULL) +
  
  scale_fill_manual(
    values = c("mistyrose", "lightblue"),
    labels = c("Max_temp", "Min_temp")
  ) +
  
  theme_bw() +
  theme(legend.position = "bottom")


