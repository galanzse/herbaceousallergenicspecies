

# FIGURE 3

library(tidyverse)


# parametros por estacion y type
parametros = read.csv("results/parametros.txt", sep="") %>%
  dplyr::select(-en.jd, -sm.ps, pk.val, -pk.jd, -daysth, -pk.val)
colnames(parametros)[colnames(parametros)=='seasons'] = 'year'


# scatter plot de valores promedio
annual_pollen_summary2 <- parametros %>%
  group_by(site, type) %>%
  summarise(
    SOP_m = median(st.jd, na.rm = TRUE),
    SOP_d = mad(st.jd, na.rm = TRUE),
    LOP_m = median(ln.ps, na.rm = TRUE),
    LOP_d = mad(ln.ps, na.rm = TRUE),
    APIn_m = median(sm.tt, na.rm = TRUE),
    APIn_d = mad(sm.tt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = 3:8, names_to = "name", values_to = "value") %>%
  mutate(
    parameter = sub("_(m|d)$", "", name),
    summary = ifelse(grepl("_m$", name), "median", "mad"),
    name=NULL
  ) %>%
  pivot_wider(names_from = summary, values_from = value)

annual_pollen_summary2$parameter = factor(annual_pollen_summary2$parameter, levels=c('SOP','LOP','APIn'))

ggplot(aes(x=site, y=median, color=type, shape=type, group=type), data=annual_pollen_summary2) +
  geom_point(position=position_dodge(width = 0.7), size=1.3) +
  geom_errorbar(
    aes(ymin = ifelse(parameter == "APIn", pmin(median - mad, 2000), median - mad),
        ymax = ifelse(parameter == "APIn", pmin(median + mad, 2000), median + mad)),
                position=position_dodge(width = 0.7), linewidth = 0.3, width = 0.3) +
  facet_wrap(~parameter, scales='free_y') +
  theme_bw() +
  theme(legend.position='top',
        axis.title.x = element_blank(),
        axis.text.x=element_text(angle=45, hjust=1, size=8))


