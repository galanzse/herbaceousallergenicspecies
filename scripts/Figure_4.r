

# FIGURE 4

library(tidyverse)

# calculo correlaciones
tendencias = read.csv("results/parametros.txt", sep="") %>%
  pivot_longer(cols=c(st.jd, ln.ps, sm.tt), names_to='fenofase') %>%
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

# el metodo da igual para sm.tt
tendencias$method[tendencias$fenofase=='sm.tt'] <- 'clinical'
tendencias = unique(tendencias)

# añadir significacion
tendencias$sig <- ifelse(tendencias$p.val < 0.05, "sig.", "non sig.")

# ordeno parametros
tendencias$fenofase = as.factor(tendencias$fenofase)
levels(tendencias$fenofase) = c('LOP', 'APIn', 'SOP')
tendencias$fenofase = factor(tendencias$fenofase, levels=c('SOP', 'LOP', 'APIn'))

# plot
ggplot(tendencias, aes(x = fenofase, y = rho)) +
  geom_boxplot() +
  geom_jitter(
    aes(colour = sig, shape = method),
    width = 0.2,
    alpha = 0.7,
    size = 1.2
  ) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = "Spearman's ρ") +
  facet_wrap(~type, scales = "free_y", nrow = 1) +
  theme_bw() +
  theme(legend.position = "bottom")

# tabla
table(tendencias$sig, tendencias$fenofase, tendencias$type)


# # plot: ver diferencias entre sitios
# ggplot(aes(x=site, y=rho, colour=sig, group=type),
#        data=tendencias[tendencias$fenofase=='sm.tt',]) +
#   geom_boxplot() +
#   geom_jitter(width=0.2, alpha=0.7, size=1.2) +
#   labs(title = "Tendencias interanuales de parametros fenologicos", x=NULL, y="Spearman's ρ") +
#   geom_hline(yintercept = 0) +
#   # facet_wrap(~site, scales='free_y', nrow=11) +
#   theme_bw() +
#   theme(legend.position='bottom')



# Cuantificar aumento de sm.sp en PLAN y URTI

temp = read.csv("results/fenofases.txt", sep="")  %>%
  subset(type %in% c('PLAN','URTI') & method=='percentage') %>%
  dplyr::select(type, site, seasons, sm.tt) %>%
  mutate(period = if_else(seasons < 2005, "first", "second")) %>%
  group_by(type, site, period) %>%
  summarise(median_api=median(sm.tt, na.rm=T),
            mad_api=mad(sm.tt, na.rm=T))

ggplot(aes(x=period, y=median_api, group=site, color=site),
       data=temp) +
  geom_jitter(width=0.1, alpha=0.7, size=1.2) +
  geom_line() +
  labs(x='Period', y='sm.tt') +
  facet_wrap(~type, scales='free_y') +
  theme_bw() +
  theme(legend.position='bottom') +
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         shape=guide_legend(nrow=2, byrow=TRUE))


