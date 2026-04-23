

# FIGURE 2

library(tidyverse)


# fenofases por estacion y type
fenofases = read.csv("results/fenofases.txt", sep="") %>%
  dplyr::select(-ln.ps, -sm.tt, pk.val, -pk.jd, -daysth, -pk.val)
colnames(fenofases)[colnames(fenofases)=='seasons'] = 'year'

annual_pollen_summary <- fenofases %>%
  group_by(site, type, year) %>%
  summarise(
    st.jd = median(st.jd, na.rm=T),
    en.jd = median(en.jd, na.rm=T),
    sm.ps = median(sm.ps, na.rm=T)
  ) %>%
  pivot_longer(cols=c(st.jd, en.jd, sm.ps), names_to='fenofase')

# exploramos diferencias entre estaciones 1
ggplot(aes(x=site, y=value, fill=site),
       data=annual_pollen_summary[annual_pollen_summary$fenofase=='sm.ps',]) +
  geom_boxplot() +
  labs(x=NULL, y="APIn") +
  scale_y_continuous(limits=c(0, 3000), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x=element_blank(), legend.position='bottom') +
  guides(fill=guide_legend(nrow=1, byrow = TRUE)) +
  facet_grid( ~ type, scales='free_y')


