

# Analizamos las tendencias en las series de polen


library(tidyverse)


# datos de clima
clima_data <- read.csv("results/all_climate_data.txt", sep="")
clima_data_summary <- read.csv("results/climate_data_summary.txt", sep="")


# descriptivos
clima_data_long = clima_data %>%
  pivot_longer(cols = c(tmed, prec, tmin, tmax, velmedia, dir),
               names_to='meteo_var', values_to='value')


# calculamos medias mensuales por estacion y variable meteorologica
monthly_climate_summary <- clima_data_long %>%
  group_by(month, nombre, meteo_var) %>%
  summarise(
    monthly_average = mean(value, na.rm = TRUE),
    .groups = 'drop'
  )

monthly_climate_summary$meteo_var <- as.factor(monthly_climate_summary$meteo_var)
monthly_climate_summary$meteo_var <- factor(monthly_climate_summary$meteo_var,
                                            c("tmax", "tmed", "tmin", "prec", "velmedia", "dir"))

ggplot(aes(x=month, y=monthly_average, colour=nombre), data=monthly_climate_summary) +
  geom_point() +
  geom_smooth(method='loess', span=0.5, se=F) +
  labs(title = "Medias mensuales por estación", x="mes", y="valor") +
  scale_x_continuous(limits=c(0,12), expand = c(0,0), breaks = seq(1, 12, by=1)) +
  theme_bw() +
  theme(legend.position='top') +
  facet_wrap(~meteo_var, scales='free_y')


# calculamos medias anuales por estacion polinica, estacion del año y variable meteorologica
annual_climate_summary <- clima_data_long %>%
  group_by(nombre, season, season_year, meteo_var) %>%
  summarise(
    year_average = mean(value, na.rm = TRUE),
    .groups = 'drop'
  )

annual_climate_summary$meteo_var <- as.factor(annual_climate_summary$meteo_var)
annual_climate_summary$meteo_var <- factor(annual_climate_summary$meteo_var,
                                            c("tmax", "tmed", "tmin", "prec", "velmedia", "dir"))

# hay un claro aumento de tmax, tmed y tmin a lo largo de la serie temporal
ggplot(aes(x=season_year, y=year_average, colour=nombre, shape=season),
       data = annual_climate_summary) +
  geom_point() +
  geom_smooth(method='loess', span=1, se=F) +
  labs(title = "Media anual por estación polínica y estación", x="año", y="media") +
  scale_x_continuous(limits=c(1994, 2026), expand = c(0, 0)) +
  theme_bw() +
  facet_wrap(~ meteo_var, scales='free_y')


# exploramos diferencias entre estaciones
ggplot(aes(x=nombre, y=year_average, fill=season),
       data=annual_climate_summary) +
  geom_boxplot() +
  labs(title=NULL, x=NULL, y="value") +
  theme_bw() +
  theme(legend.position='top') +
  coord_flip() +
  facet_wrap(~ meteo_var, scales='free_x')


# las precipitaciones muestran gran variabilidad interanual
ggplot(aes(x=season_year, y=year_average, colour=nombre),
       data = annual_climate_summary[annual_climate_summary$meteo_var=='prec',]) +
  geom_point() +
  geom_smooth(method='loess', span=1.5, se=F) +
  labs(title = "Media anual por estación polínica y estación", x="año", y="precipitacion media") +
  scale_x_continuous(limits=c(1994, 2026), expand = c(0, 0)) +
  theme_bw() +
  facet_wrap(~ season, scales='free_y')


# exploramos los vientos por estaciones
data_winds = clima_data %>%
  pivot_longer(cols=c(tmed, prec, tmin, tmax, velmedia, dir), names_to='meteo_var', values_to='value') %>%
  subset(meteo_var=='dir') %>%
  dplyr::select(nombre, fecha, season, value)
colnames(data_winds)[colnames(data_winds)=='value'] = 'wind_direction'

ggplot(data_winds, aes(x=wind_direction, fill=season)) +
  geom_histogram(
    binwidth = 45,   # mismo ancho que tus sectores
    boundary = 0
  ) +
  coord_polar(start = 0) +
  scale_x_continuous(
    limits = c(0, 360),
    breaks = seq(0, 360, by = 45)
  ) +
  labs(title = "Histograma circular de dirección del viento",
       x = "Dirección (grados)",
       y = "Frecuencia") +
  theme_bw() +
  facet_wrap(~nombre)


