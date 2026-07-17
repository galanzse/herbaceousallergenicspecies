

# FIGURE 4

library(tidyverse)
library(Kendall)
library(modifiedmk)

# calculo correlaciones
tendencias <- read.csv("results/parametros.txt", sep = "") %>%
  pivot_longer(cols = c(st.jd, ln.ps, sm.tt), names_to = "fenofase") %>%
  group_by(type, site, method, fenofase) %>%
  reframe({
    n <- sum(!is.na(value) & !is.na(seasons))
    
    if (n > 2) {
      
      test.sp <- cor.test(value, seasons, method = "spearman")
      test.mk <- MannKendall(value)
      sen <- tryCatch(
        mmkh(value)["Sen's slope"],
        error = function(e) NA_real_
      )
      
      data.frame(
        sp.rho  = unname(test.sp$estimate),
        sp.pval = test.sp$p.value,
        mk.tau  = unname(test.mk$tau),
        mk.pval = test.mk$sl,
        sen.slope = sen
      )
      
    } else {
      
      data.frame(
        sp.rho = NA_real_,
        sp.pval = NA_real_,
        mk.tau = NA_real_,
        mk.pval = NA_real_,
        sen.slope = NA_real_
      )
      
    }
  }) %>%
  na.omit()

# el metodo da igual para sm.tt
tendencias$method[tendencias$fenofase=='sm.tt'] <- 'clinical'
tendencias = unique(tendencias)

# relacion entre rho y tau
plot(tendencias$mk.tau, tendencias$sp.rho); abline(0,1, col='blue'); abline(v=0, lty=2)
table(tendencias$sp.pval<0.05, tendencias$mk.pval<0.05)

# son casi identicos, seguimos con Mann-Kendall: añadir significacion
tendencias$mk.sig <- ifelse(tendencias$mk.pval < 0.05, "sig.", "non sig.")

# ordeno parametros
tendencias$fenofase = as.factor(tendencias$fenofase)
levels(tendencias$fenofase) = c('LOP', 'APIn', 'SOP')
tendencias$fenofase = factor(tendencias$fenofase, levels=c('SOP', 'LOP', 'APIn'))


# plot para articulo

tendencias$sen.slope[tendencias$fenofase=='SOP' & tendencias$sen.slope>5] = NA
tendencias$sen.slope[tendencias$fenofase=='SOP' & tendencias$sen.slope< c(-5)] = NA
tendencias$sen.slope[tendencias$fenofase=='LOP' & tendencias$sen.slope>5] = NA
tendencias$sen.slope[tendencias$fenofase=='LOP' & tendencias$sen.slope< c(-5)] = NA
tendencias$sen.slope[tendencias$fenofase=='APIn' & tendencias$sen.slope>100] = NA

ggplot(tendencias, aes(x = type, y = sen.slope)) +
  geom_boxplot() +
  geom_jitter(
    aes(colour = mk.sig, shape = site),
    width = 0.2,
    alpha = 0.7,
    size = 1.2
  ) +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 15)) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = "Sen's slope") +
  facet_wrap(~fenofase, scales = "free_y", nrow = 1) +
  theme_bw() +
  theme(legend.position = "bottom")


median(tendencias$sen.slope[tendencias$type=='URTI' & tendencias$fenofase=='SOP'], na.rm=T)
mad(tendencias$sen.slope[tendencias$type=='URTI' & tendencias$fenofase=='SOP'], na.rm=T)
median(tendencias$sen.slope[tendencias$type=='PLAN' & tendencias$fenofase=='APIn'], na.rm=T)
mad(tendencias$sen.slope[tendencias$type=='PLAN' & tendencias$fenofase=='APIn'], na.rm=T)
median(tendencias$sen.slope[tendencias$type=='URTI' & tendencias$fenofase=='APIn'], na.rm=T)
mad(tendencias$sen.slope[tendencias$type=='URTI' & tendencias$fenofase=='APIn'], na.rm=T)


