

# Importamos datos de clima


source('scripts/0.1_pollen_stations.r')


# datos de estaciones AEMET de Madrid
load("data/aemet/meteolist_long_Madrid_13-01-2026.RData")

# corregimos el nombre
climate_data = datos_totales
rm(datos_totales)

# nos quedamos con datos del rango 1993-2025
climate_data = climate_data %>% subset(between(fecha, as.Date('1993-01-01'), as.Date('2025-12-31')))

# import madrid shapefile
v_madrid <- mapSpain:::esp_get_ccaa('madrid',epsg="4326") %>% terra::vect() %>% project('epsg:25830')

# y con estaciones relevantes
aemet_points = rbind(terra::vect('data/aemet/aemet_automaticas/Estaciones_Automaticas.shp'),
                     terra::vect('data/aemet/aemet_completas/Estaciones_Completas.shp'),
                     terra::vect('data/aemet/aemet_termometricas/Estaciones_Termometricas.shp'),
                     terra::vect('data/aemet/aemet_pluviometricas/Estaciones_Pluviometricas.shp')) %>%
  terra::project('epsg:25830') %>%
  terra::crop(v_madrid)

# eliminamos estaciones cercanas que no figuran en el archivo de Jorge
aemet_points = aemet_points[!(aemet_points$NOMBRE %in% c("MADRID/BARAJAS","MAJADAHONDA (MAFRE)","MADRID (SORIA)","TORRELODONES (MONTE PEGUERINOS)","COLEGIO PABLO PICASO")),]


# retenemos estaciones AEMET cerca de estaciones PALINOCAM
idx <- nearest(v_pollen_stations, aemet_points)
aemet_points <- aemet_points[idx, ]

# corregimos nombres
climate_data$nombre <- as.factor(climate_data$nombre)
aemet_points$NOMBRE <- as.factor(aemet_points$NOMBRE)
levels(aemet_points$NOMBRE) <- c("ALPEDRETE", "ARANJUEZ", "MADRID, CIUDAD UNIVERSITARIA", "MADRID, RETIRO", "GETAFE", "TORREJÓN DE ARDOZ", "POZUELO DE ALARCÓN", "SAN SEBASTIÁN DE LOS REYES")

# seleccionamos dichas estaciones, añadimos El Goloso porque Angel la incluye en su tesis
climate_data = climate_data %>% subset(nombre %in% c(levels(aemet_points$NOMBRE), 'MADRID, EL GOLOSO'))


# corregimos variables
str(climate_data)
climate_data = climate_data[,c('fecha','nombre','tmed','prec','tmin','tmax','velmedia','dir')]
climate_data$fecha <- as.Date(climate_data$fecha)
climate_data$nombre <- as.factor(climate_data$nombre)
climate_data$tmed <- as.numeric(climate_data$tmed)
climate_data$prec <- as.numeric(climate_data$prec)
climate_data$tmin <- as.numeric(climate_data$tmin)
climate_data$tmax <- as.numeric(climate_data$tmax)
climate_data$velmedia <- as.numeric(climate_data$velmedia)
climate_data$dir <- as.numeric(climate_data$dir)
climate_data$dir[climate_data$dir>36] <- NA
climate_data$dir <- climate_data$dir * 10

# parece que los datos están bien
boxplot(climate_data[,c('tmed', 'prec', 'tmin', 'tmax', 'velmedia', 'dir')])
cor.test(climate_data$tmed, climate_data$tmin, use="pairwise.complete.obs", method="spearman")



# CONTROL DE CALIDAD ####

# añadimos variables relativas a la estación para agrupar posteriormente
climate_data <- climate_data %>%
  mutate(
    year = year(fecha),
    month = month(fecha),
    
    hydro_year = case_when(
      month %in% 1:5 ~ year,
      month %in% 6:12 ~ year + 1
    ),
    
    season = case_when(
      month %in% c(12,1,2) ~ "DJF",
      month %in% c(3,4,5) ~ "MAM",
      month %in% c(6,7,8) ~ "JJA",
      month %in% c(9,10,11) ~ "SON"
    ),
    
    season_year = ifelse(month == 12, year + 1, year)
  )

# reorganizo columnas
climate_data <- climate_data[,c("nombre", "fecha",  "year", "hydro_year", "season_year", "season", "month", "tmed", "prec", "tmin", "tmax", "velmedia", "dir")]


# # guardo
# write.table(climate_data, 'results/all_climate_data.txt')


# calculamos la cobertura estacional (proporcion de dias para los que hay datos por estacion)
valid_combinations <- climate_data %>%
  group_by(season_year, season, nombre) %>%
  summarise(
    total_days = 92,
    tmed = round(sum(!is.na(tmed)) / total_days * 100, 1),
    prec = round(sum(!is.na(prec)) / total_days * 100, 1),
    tmin = round(sum(!is.na(tmin)) / total_days * 100, 1),
    tmax = round(sum(!is.na(tmax)) / total_days * 100, 1),
    velmedia = round(sum(!is.na(velmedia)) / total_days * 100, 1),
    dir = round(sum(!is.na(dir)) / total_days * 100, 1)
  ) %>%
  dplyr::select(-total_days) %>%
  pivot_longer(cols = c(tmed, prec, tmin, tmax, velmedia, dir),
               names_to='meteo_var', values_to='coverage') %>%
  # retenemos trimestres con suficientes datos (~70 dias)
  filter(coverage>=77) %>% dplyr::select(-coverage)

# retenemos combinaciones validas
climate_data_long <- climate_data %>%
  dplyr::select(fecha, nombre, season, season_year, tmed, prec, tmin, tmax, velmedia, dir) %>%
  pivot_longer(cols = c(tmed, prec, tmin, tmax, velmedia, dir),
               names_to = "meteo_var", values_to = "value")

climate_data_valid <- left_join(valid_combinations, climate_data_long)

# devolvemos a formato ancho y hacemos summary
climate_data_summary = climate_data_valid %>%
  pivot_wider(names_from="meteo_var", values_from="value") %>%
  group_by(nombre, season, season_year) %>%
  summarise(
    tmax_t = sum(tmax, na.rm = TRUE),
    tmin_t = sum(tmin, na.rm = TRUE),
    prec_t = sum(prec, na.rm = TRUE),
    tmed_m = mean(tmed, na.rm = TRUE),
    tmax_m = mean(tmax, na.rm = TRUE),
    tmin_m = mean(tmin, na.rm = TRUE),
    .groups = "drop"
  )



# RELACIONAMOS AEMET Y PALINOCAM Y GUARDAMOS ####

# creamos nueva variable site
climate_data_summary <- climate_data_summary %>%
  filter(nombre != 'SAN SEBASTIÁN DE LOS REYES') %>%
  mutate(site = case_when(
    nombre == "TORREJÓN DE ARDOZ" ~ "ALCA",
    nombre == "MADRID, EL GOLOSO" ~ "ALCO",
    nombre == "ARANJUEZ" ~ "ARAN",
    nombre == "MADRID, RETIRO" ~ "AYTM",
    nombre == "MADRID, CIUDAD UNIVERSITARIA" ~ "FACF",
    nombre == "GETAFE" ~ "GETA",
    nombre == "POZUELO DE ALARCÓN" ~ "ROZA",
    nombre == "ALPEDRETE" ~ "VILL"
  ))

# duplicamos ALCA y GETA para cubrir COSL y LEGA
duplicados <- climate_data_summary %>%
  filter(site %in% c("ALCA", "GETA","AYTM")) %>%
  mutate(site = case_when(
    site == "ALCA" ~ "COSL",
    site == "GETA" ~ "LEGA"
  ))

# unimos
climate_data_summary <- bind_rows(climate_data_summary, duplicados)


# # guardamos
# write.table(climate_data_summary, 'results/climate_data_summary.txt')


