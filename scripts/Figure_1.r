

# FIGURE 1

library(tidyverse)
library(effectsize)
library(car)


# concentracion de polen diaria x tpp
daily_averages <- read.csv("results/curated_pollen_data.txt", sep="") %>%
  mutate(doy=as.numeric(format(as.Date(date), "%j"))) %>%
  group_by(doy, type) %>%
  summarise(mean=mean(gr_x_m3, na.rm=T))

# importo fenofases
feno = read.csv("results/fenofases.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, st.jd, en.jd) %>%
  pivot_longer(cols=c(st.jd, en.jd), names_to='fenofase', values_to='feno_value') %>%
  group_by(type, fenofase) %>%
    summarise(feno_value = mean(feno_value, na.rm=T))
colnames(feno)[2] = 'parameter'
feno$parameter <- factor(feno$parameter, c("st.jd","en.jd"))
feno$y=0

ggplot(aes(x=doy, y=mean, colour=type, group=type), data=daily_averages) +
  geom_point(size=0.1) +
  geom_smooth(method='loess', span=0.1, se=F) +
  labs(x="doy", y="gr x m-3") +
  scale_x_continuous(limits=c(0,365), expand=c(0, 0), breaks=seq(0, 365, by=50)) +
  scale_y_continuous(limits=c(0, max(daily_averages$mean+5)),
                     expand=expansion(mult = c(0.025, 0.025))) +
  theme_bw() +
  # feno
  geom_point(data=feno, aes(feno_value,y,shape=parameter), size=3, stroke=0.9) +
  scale_shape_manual(values = c("st.jd"=2, "en.jd"=6))



# estadisticos
read.csv("results/fenofases.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, st.jd, en.jd, sm.ps) %>%
  pivot_longer(cols=c(st.jd, en.jd, sm.ps), names_to='fenofase', values_to='feno_value') %>%
  group_by(type, fenofase) %>%
  summarise(mean = mean(feno_value, na.rm=T),
            sd = sd(feno_value, na.rm=T)) %>%
  as.data.frame()


  
# modelos
mod_data = read.csv("results/fenofases.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, st.jd, en.jd, sm.ps) %>%
  subset(type!='ARTE' & method!='logistic')

mod_data$type = as.factor(mod_data$type)
mod_data$site = as.factor(mod_data$site)
mod_data$method = as.factor(mod_data$method)
mod_data$seasons <- as.numeric(mod_data$seasons)
mod_data$st.jd = as.numeric(mod_data$st.jd)
mod_data$en.jd = as.numeric(mod_data$en.jd)
mod_data$sm.ps = as.numeric(mod_data$sm.ps)

str(mod_data)

mod <- lm(st.jd ~ type + site + method + seasons, data=mod_data)
Anova(mod, type="II") 
t = Anova(mod, type="II"); round(t$`Sum Sq`/sum(t$`Sum Sq`), 2)

mod <- lm(en.jd ~ type + site + method + seasons, data=mod_data)
Anova(mod, type="II") 
t = Anova(mod, type="II"); round(t$`Sum Sq`/sum(t$`Sum Sq`), 2)

mod <- lm(sm.ps ~ type + site + method + seasons, data=mod_data)
Anova(mod, type="II") 
t = Anova(mod, type="II"); round(t$`Sum Sq`/sum(t$`Sum Sq`), 2)


