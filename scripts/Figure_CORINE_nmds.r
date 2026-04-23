
library(ggplot2)
library(ggrepel)
library(vegan)

nmds = readRDS('results/nmds.rds')

sites <- as.data.frame(scores(nmds, display = "sites"))
species <- as.data.frame(scores(nmds, display = "species"))

sites$label <- rownames(sites)
sites[, c("site", "year")] <- do.call(rbind, strsplit(sites$label, "_"))
sites$afterchange = 'no'
sites$afterchange[sites$label%in%c('ALCO_2012','ALCO_2018','ARAN_2000','ARAN_2012','ARAN_2018','COSL_2012','COSL_2018','VILL_2018','AYTM_2006','AYTM_2012','AYTM_2018')] = 'yes'
species$label <- rownames(species)

ggplot() +
  geom_point(data = sites, aes(x = NMDS1, y = NMDS2)) +
  geom_text_repel(data = sites, size=3, aes(x = NMDS1, y = NMDS2, label = label, color=afterchange)) +
  geom_text_repel(data = species, size=3,
                  aes(x = NMDS1, y = NMDS2, label = label),
                  color = "black") +
  theme(legend.position = "none") +
  guides(fill = "none", color = "none", shape = "none") +
  # ggtitle('NMDS on recclassified CORINE & 10km buffer around each pollen station') +
  theme_bw()


