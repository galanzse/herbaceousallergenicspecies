
# Figure 5

source('scripts/3.1_pollen_x_climate.r')

climate_trends$meteo_sum <- factor(
  climate_trends$meteo_sum,
  levels = c("tmed_m", "prec_t"),
  labels = c("Temperature", "Precipitation")
)

colnames(climate_trends)[colnames(climate_trends)%in%'meteo_sum'] = 'Meteo'
colnames(climate_trends)[colnames(climate_trends)%in%'sig'] = 'Sig.'

ggplot(climate_trends,
       aes(x = fenofase,
           y = rho,
           fill = Meteo)) +
  
  geom_boxplot(
    position = position_dodge(width = 0.8),
    outlier.shape = NA,
    alpha = 0.6
  ) +
  
  geom_point(
    aes(colour = Sig.,
        group = Meteo,
        shape=method),
    position = position_jitterdodge(
      jitter.width = 0.2,
      dodge.width = 0.8
    ),
    alpha = 0.7,
    size = 1.2
  ) +

  geom_hline(yintercept = 0) +
  
  facet_grid(season ~ type, scales = "free_x") +
  
  labs(y="Spearman's p", x=NULL) +
  
  scale_fill_manual(
    values = c("grey99", "grey50"),
    labels = c("Temperature", "Precipitation")
  ) +
  
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE),
         shape = guide_legend(nrow = 2, byrow = TRUE),
         fill = guide_legend(nrow = 2, byrow = TRUE))


