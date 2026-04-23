

# Cruzamos la base de datos de polen con la de clima


library(tidyverse)


# datos de clima
climate <- read.csv("results/clima_data_summary.txt", sep="") %>%
  mutate(year = season_year) %>% 
  dplyr::select(site, year, season, prec_t, tmed_m) %>%
  pivot_longer(cols=c(prec_t, tmed_m),
               names_to='meteo_sum', values_to='meteoval')
str(climate)
climate$site <- as.factor(climate$site)
climate$season <- as.factor(climate$season)
climate$meteoval[climate$meteoval==0] <- NA


# datos de polen
fenofases <- read.csv("results/fenofases.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, st.jd, en.jd, sm.ps)
colnames(fenofases)[colnames(fenofases)=='seasons'] = 'year'

# unimos
climatexpollen <- merge(climate, fenofases, by=c('site','year'))

# eliminamos temperaturas acumuladas
climatexpollen <- climatexpollen[climatexpollen$meteo_sum %in% c('prec_t','tmed_m'),]


# calculamos tendencias
climate_trends = climatexpollen %>%
  pivot_longer(cols=c(st.jd, en.jd, sm.ps), names_to='fenofase', values_to='fenoval') %>%
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
climate_trends$fenofase <- factor(climate_trends$fenofase, levels=c('st.jd', 'en.jd', 'sm.ps'))

# eliminamos ARTE
climate_trends <- climate_trends[climate_trends$type!='ARTE',]

# eliminamos comparaciones
climate_trends$rho[climate_trends$season=='SON' & climate_trends$fenofase=='st.jd'] <- NA
climate_trends$rho[climate_trends$season=='JJA' & climate_trends$fenofase=='st.jd'] <- NA
climate_trends$rho[climate_trends$season=='DJF' & climate_trends$fenofase=='en.jd'] <- NA

# plots
ggplot(aes(x=fenofase, y=rho, group=fenofase, colour=sig, shape=method),
       data=climate_trends[climate_trends$meteo_sum=='prec_t',]) +
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.7, size=1.2) +
  geom_hline(yintercept=0) +
  facet_wrap(~season+type, scales='free_x', nrow=4) +
  labs(title='Precipitation', y='Spearman ρ') +
  theme_bw() +
  theme(legend.position='bottom')

ggplot(aes(x=fenofase, y=rho, group=fenofase, colour=sig, shape=method),
       data=climate_trends[climate_trends$meteo_sum=='tmed_m',]) +
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.7, size=1.2) +
  geom_hline(yintercept=0) +
  facet_wrap(~season+type, scales='free_x', nrow=4) +
  labs(title='Mean temperature', y='Spearman ρ') +
  theme_bw() +
  theme(legend.position='bottom')

# tabla
temp = climate_trends[climate_trends$season=='DJF' & climate_trends$type=='URTI',]
table(temp$sig, temp$fenofase, temp$meteo_sum)


