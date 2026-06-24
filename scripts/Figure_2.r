

# FIGURE 2

library(tidyverse)
library(effectsize)
library(car)


# concentracion de polen diaria x tpp
daily_averages <- read.csv("results/curated_pollen_data.txt", sep="") %>%
  mutate(doy=as.numeric(format(as.Date(date), "%j"))) %>%
  group_by(doy, type) %>%
  summarise(mean=mean(gr_x_m3, na.rm=T))

# importo parametros
feno = read.csv("results/parametros.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, st.jd) %>%
  pivot_longer(cols=c(st.jd), names_to='fenofase', values_to='feno_value') %>%
  group_by(type, fenofase) %>%
    summarise(feno_value_median = median(feno_value, na.rm=T),
              feno_value_mad = mad(feno_value, na.rm=T))
colnames(feno)[2] = 'parameter'
feno$parameter <- 'SOP'
feno$y=0

ggplot(aes(x=doy, y=mean, colour=type, group=type), data=daily_averages) +
  geom_point(size=0.1) +
  geom_smooth(method='loess', span=0.1, se=F) +
  labs(x="doy", y="Airborne pollen concentration (pollen m-3)") +
  scale_x_continuous(limits=c(0,365), expand=c(0, 0), breaks=seq(0, 365, by=50)) +
  scale_y_continuous(limits=c(0, max(daily_averages$mean+5)),
                     expand=expansion(mult = c(0.025, 0.025))) +
  theme_bw() +
  # feno
  geom_point(data=feno, aes(feno_value_median, y, shape=parameter), size=3, stroke=0.9) +
  scale_shape_manual(values = c("SOP"=2, "LOP"=6))



# estadisticos
read.csv("results/parametros.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, st.jd, ln.ps, sm.tt) %>%
  pivot_longer(cols=c(st.jd, ln.ps, sm.tt), names_to='fenofase', values_to='feno_value') %>%
  group_by(type, fenofase) %>%
  summarise(median = median(feno_value, na.rm=T),
            mad = mad(feno_value, na.rm=T)) %>%
  as.data.frame()


