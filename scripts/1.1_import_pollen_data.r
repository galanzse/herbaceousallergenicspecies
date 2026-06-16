


# Importar datos de polen y unificarlos para análisis posteriores


library(tidyverse)
library(readxl)
library(terra)
# library(mapSpain)
library(AeRobiology)



# import yearly pollen data
pollen_data <- list.files('data/pollen/series', full.names = TRUE)

# read all files into a list
pollen_data <- lapply(pollen_data, read.csv2)

# combine into one dataframe
pollen_data <- do.call(rbind, pollen_data)
table(is.na(pollen_data$granos_de_polen_x_metro_cubico))

# me cargo Subiza
pollen_data = pollen_data[pollen_data$captador!='ALER',]

# check variables
str(pollen_data)

# colnames
colnames(pollen_data) <- c("site", "date", "type", "gr_x_m3")

# set date format
pollen_data$date <- as.Date(pollen_data$date)
pollen_data$month <- month(pollen_data$date)
pollen_data$year <- year(pollen_data$date)

# filter pollen type
pollen_data = pollen_data[pollen_data$type %in% c("Artemisia", "Plantago", "Quenopodi?ceas/Amarant?ceas", "Quenopodi\xe1ceas/Amarant\xe1ceas", "Rumex (Acederas)", "Urticaceae (Ortigas)"),]

# correct factor
pollen_data <- pollen_data %>%
  mutate(
    type = recode(
      type,
      "Artemisia" = "ARTE",
      "Plantago" = "PLAN",
      "Quenopodi?ceas/Amarant?ceas" = "AMAR",
      "Quenopodi\xe1ceas/Amarant\xe1ceas" = "AMAR",
      "Rumex (Acederas)" = "RUME",
      "Urticaceae (Ortigas)" = "URTI"
    )
  )



# CONTROL DE CALIDAD 1 ####

# calculamos medias diarias por estacion y tipo polinico
pollen_data = pollen_data %>% group_by(site, date, type, month, year) %>%
  summarise(gr_x_m3 = mean(gr_x_m3, na.rm=T))

# calculamos la cobertura mensual (proporcion de dias para los que hay datos)
monthly_quality <- pollen_data %>%
  mutate(month_start = floor_date(date, "month")) %>%
  group_by(site, type, month, year, month_start) %>%
  summarise(
    n_days_recorded = sum(!is.na(gr_x_m3)),
    days_in_month = days_in_month(unique(month_start)),
    coverage_pct = round((n_days_recorded / days_in_month) * 100, 1),
    .groups = 'drop'
  ) %>%
  dplyr::select(-month_start)

# histograma de porcentaje de dias que no son NA por mes
hist(monthly_quality$coverage_pct)

# como tenemos diferentes taxones, identificamos coberturas>70%, la ventana es dependiente del tipo polinico
tpp_rules <- tibble(
  type = c("ARTE", "PLAN", "AMAR", "RUME", "URTI"),
  start = c(7, 4, 4, 4, 3),
  end   = c(11, 6, 10, 6, 7)
)

valid_combinations <- monthly_quality %>%
  inner_join(tpp_rules, by = "type") %>%
  group_by(year, site, type) %>%
  filter(
    all(seq(first(start), first(end)) %in% month) &
      all(coverage_pct[month %in% seq(first(start), first(end))] >= 70)
  ) %>%
  distinct(year, site, type) %>%
  ungroup()

# filtramos combinaciones de año x sitio x tipo polinico con cobertura > 70% para cada mes entre febrero y septiembre
pollen_data_filt <- pollen_data %>%
  inner_join(valid_combinations, by = c("year", "site", "type"))



# CONTROL DE CALIDAD 2: ver z-score de los primeros años (menos fiables) ####
annual_pollen_summary <- pollen_data %>%
  group_by(year, site, type) %>%
  summarise(
    total_pollen_year = sum(gr_x_m3, na.rm = TRUE),
    .groups = 'drop'
  )

quality_stats <- annual_pollen_summary %>%
  group_by(site, type) %>%
  mutate(
    mean_hist = mean(total_pollen_year, na.rm = TRUE),
    sd_hist = sd(total_pollen_year, na.rm = TRUE),
    z_score = (total_pollen_year - mean_hist) / sd_hist
  ) %>%
  select(site, year, type, z_score) %>%
  arrange(desc(abs(z_score))) %>%
  subset(year%in%c(1994,1995) & z_score>2)

print(quality_stats)
table(quality_stats$z_score>2)

# eliminamos primeros años con demasiada variabilidad
pollen_data_filt <- pollen_data_filt %>%
  anti_join(quality_stats[,c("year", "site", "type")], by = c("year", "site", "type"))



# INTERPOLACIONES DIARIAS ####

# cambiamos series a formato ancho, y añadimos los dias que faltan
pollen_data_wide <- pollen_data_filt %>%
  dplyr::select(site, date, type, gr_x_m3) %>%
  pivot_wider(names_from=type, values_from=gr_x_m3) %>%
  ungroup() %>%
  complete(date = seq.Date(min(date), max(date), by = "day")) %>%
  dplyr::select(site, date, ARTE, PLAN, AMAR, RUME, URTI) %>%
  arrange(date)

# convertimos en lista
pollen_data_wide <- split(pollen_data_wide, pollen_data_wide$site)

# interpolamos con el metodo 'movingmean'
int_pollen_data <- lapply(pollen_data_wide, function(df) interpollen(df[,-1], method="movingmean", factor=2, plot=F))
int_pollen_data <- lapply(int_pollen_data, function(df) {
  if (names(df)[1] == "Date") {
    names(df)[1] <- "date"
  }
  df
})


# ejemplo de serie interpolada, plot
ggplot(aes(x=date, y=PLAN), data=int_pollen_data$VILL) + geom_point(colour='red', size=0.7) +
  geom_point(data=pollen_data_wide$VILL, color='grey', size=0.7) +
  theme_bw()


# formato largo
int_pollen_data <- bind_rows(int_pollen_data, .id="site") %>%
  pivot_longer(3:7, names_to='type', values_to='gr_x_m3')

# corregimos tabla
int_pollen_data$month <- month(int_pollen_data$date)
int_pollen_data$year <- year(int_pollen_data$date)

int_monthly_pollen_summary <- int_pollen_data %>%
  group_by(month, site, type) %>%
  summarise(
    total_pollen_month = sum(gr_x_m3, na.rm = TRUE),
    .groups = 'drop'
  )


# numero de dias con concentracion > 30 gr*m-3
table(int_pollen_data$gr_x_m3>30)/sum(table(int_pollen_data$gr_x_m3>30))*100
# por tipo polinico
table(int_pollen_data$gr_x_m3>30, int_pollen_data$type)/colSums(table(int_pollen_data$gr_x_m3>30, int_pollen_data$type))*100


# # save
# write.table(int_pollen_data, 'results/curated_pollen_data.txt')


