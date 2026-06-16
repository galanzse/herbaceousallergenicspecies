

# Comparacion entre métodos

library(tidyverse)
library(AeRobiology)


parametros = read.csv("results/parametros.txt", sep="") %>%
  select(type, site, method, seasons, st.jd, ln.ps) %>%
  subset(type!='ARTE')


result <- parametros %>%
  group_by(type, site, seasons) %>%
  
  # keep only groups with at least 3 rows/methods
  filter(n() >= 3) %>%
  
  # calculate group medians and distances to median
  mutate(
    med_st.jd = median(st.jd, na.rm = TRUE),
    med_ln.ps = median(ln.ps, na.rm = TRUE),

    dist_st.jd = st.jd - med_st.jd,
    dist_ln.ps = ln.ps - med_ln.ps
  ) %>%
  ungroup() %>%
  mutate(st.jd = dist_st.jd,
         ln.ps = dist_ln.ps) %>% 
  select(type, site, method, seasons, st.jd, ln.ps) %>%
  pivot_longer(cols=c(st.jd, ln.ps), names_to = 'parameter', values_to = 'dist') %>% 
  ungroup()


head(result)

result$parameter = factor(result$parameter, levels=c('st.jd', 'ln.ps'))
levels(result$parameter) = c('SOP', 'LOP')

ggplot(aes(x=type, y= dist, color=method), data=result) +
  geom_boxplot() +
  # ylim(-25, 25) +
  theme_bw() +
  labs(title=NULL, y='Number of days', x=NULL) +
  theme(legend.position = 'top') +
  facet_wrap(~parameter) # scales='free_y'


