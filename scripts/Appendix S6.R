# MS ####

# datos de clima
climate <- read.csv("results/clima_data_summary.txt", sep="") %>%
  mutate(year = season_year) %>% 
  dplyr::select(site, year, season, tmin_m, tmax_m) %>%
  pivot_longer(cols=c(tmin_m, tmax_m),
               names_to='meteo_sum', values_to='meteoval')
str(climate)
climate$site <- as.factor(climate$site)
climate$season <- as.factor(climate$season)
climate$meteoval[climate$meteoval==0] <- NA


# datos de polen
fenofases <- read.csv("results/parametros.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, st.jd, ln.ps, sm.tt)
colnames(fenofases)[colnames(fenofases)=='seasons'] = 'year'

# unimos
climatexpollen <- merge(climate, fenofases, by=c('site','year'))

# eliminamos temperaturas acumuladas
climatexpollen <- climatexpollen[climatexpollen$meteo_sum %in% c('tmin_m', 'tmax_m'),]

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

ggplot(aes(x=fenofase, y=rho, group=fenofase, colour=sig, shape=method),
       data=climate_trends[climate_trends$meteo_sum=='tmin_m',]) +
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.7, size=1.2) +
  geom_hline(yintercept=0) +
  facet_wrap(~season+type, scales='free_x', nrow=3) +
  labs(title='Seasonal average minimum temperature', y='Spearman ρ', x=NULL) +
  theme_bw() +
  theme(legend.position='bottom')

ggplot(aes(x=fenofase, y=rho, group=fenofase, colour=sig, shape=method),
       data=climate_trends[climate_trends$meteo_sum=='tmax_m',]) +
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.7, size=1.2) +
  geom_hline(yintercept=0) +
  facet_wrap(~season+type, scales='free_x', nrow=3) +
  labs(title='Seasonal average maximum temperature', y='Spearman ρ', x=NULL) +
  theme_bw() +
  theme(legend.position='bottom')