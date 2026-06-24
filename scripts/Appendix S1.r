

# Appendix S1

source('scripts/4.1_sources.r')

df <- lu_spatial_dif[,1:12]
str(lu_spatial_dif)

# Convert to long format
df_long <- df %>%
  pivot_longer(
    cols = starts_with("a_"),
    names_to = "landuse",
    values_to = "area"
  ) %>%
  
  # Relative abundance within each station-year
  group_by(site, año) %>%
  mutate(rel_abundance = area / sum(area)) %>%
  ungroup()

# Barplot
ggplot(df_long,
       aes(x = factor(año),
           y = rel_abundance,
           fill = landuse)) +
  
  geom_bar(stat = "identity") +
  
  facet_wrap(~site, ncol=3) +
  
  ylab("Relative abundance") +
  xlab(NULL) +
  
  theme_bw() +
  theme(legend.position = 'bottom') +
  guides(fill=guide_legend(nrow=3, byrow=TRUE))


# analisis NMDs

CORINE_nmds = lu_spatial_dif[,1:12] %>% as.data.frame()
rownames(CORINE_nmds) = paste0(CORINE_nmds$site, CORINE_nmds$año)
CORINE_nmds$site = NULL
CORINE_nmds$año = NULL

nmds <- metaMDS(CORINE_nmds, distance="bray", k=2, trymax=100)
plot(nmds, type = "n")
orditorp(nmds, display = "sites", labels = rownames(CORINE_nmds), cex = 0.6)
orditorp(nmds, display = "species", cex = 0.5, col = "blue")


