

# FIGURE 3

library(tidyverse)

# calculo correlaciones
tendencias = read.csv("results/fenofases.txt", sep="") %>%
  pivot_longer(cols=c(st.jd, en.jd, sm.ps), names_to='fenofase') %>%
  group_by(type, site, method, fenofase) %>%
  summarise({
    if (sum(!is.na(value) & !is.na(seasons)) > 2) {
      test <- cor.test(value, seasons, method = "spearman")
      data.frame(
        rho = unname(test$estimate),
        p.val = test$p.value
      )
    } else {
      data.frame(rho = NA, p.val = NA)
    }
  }, .groups = "drop") %>%
  na.omit()

# añadIr significacion
tendencias$sig <- ifelse(tendencias$p.val < 0.05, "sig.", "non sig.")

# ordeno parametros
tendencias$fenofase <- factor(tendencias$fenofase, levels=c("st.jd", "en.jd", "sm.ps"))

# plot
ggplot(aes(x=fenofase, y=rho, group=fenofase, colour=sig, shape=method), data=tendencias) +
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.7, size=1.2) +
  labs(x=NULL, y="Spearman ρ") +
  geom_hline(yintercept = 0) +
  facet_wrap(~type, scales='free_y', nrow=1) +
  theme_bw() +
  theme(legend.position='bottom')

# tabla
table(tendencias$sig, tendencias$fenofase, tendencias$type)


# # plot: ver diferencias entre sitios
# ggplot(aes(x=site, y=rho, colour=sig, group=type),
#        data=tendencias[tendencias$fenofase=='sm.ps',]) +
#   geom_boxplot() +
#   geom_jitter(width=0.2, alpha=0.7, size=1.2) +
#   labs(title = "Tendencias interanuales de parametros fenologicos", x=NULL, y="Spearman's ρ") +
#   geom_hline(yintercept = 0) +
#   # facet_wrap(~site, scales='free_y', nrow=11) +
#   theme_bw() +
#   theme(legend.position='bottom')



# Cuantificar aumento de sm.sp en PLAN y URTI

temp = read.csv("results/fenofases.txt", sep="")  %>%
  subset(type %in% c('PLAN','URTI')) %>%
  dplyr::select(type, site, method, seasons, sm.ps) %>%
  mutate(period = if_else(seasons < 2005, "first", "second")) %>%
  group_by(type, site, method, period) %>%
  summarise(median_api=median(sm.ps, na.rm=T),
            mad_api=mad(sm.ps, na.rm=T))

ggplot(aes(x=period, y=median_api, group=interaction(site,method), color=site, shape=method),
       data=temp) +
  geom_jitter(width=0.1, alpha=0.7, size=1.2) +
  geom_line() +
  labs(x='Period', y='Median APIn') +
  facet_wrap(~type, scales='free_y') +
  theme_bw() +
  theme(legend.position='bottom') +
  guides(color=guide_legend(nrow=3, byrow=TRUE),
         shape=guide_legend(nrow=3, byrow=TRUE))
  


