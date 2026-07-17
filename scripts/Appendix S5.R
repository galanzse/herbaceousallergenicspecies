# appendix S4
read.csv("results/parametros.txt", sep="") %>%
  dplyr::select(type, site, method, seasons, st.jd, ln.ps, sm.tt) %>%
  group_by(type, site) %>%
  summarise(st.jd_m = median(st.jd, na.rm=T),
            st.jd_d = mad(st.jd, na.rm=T),
            ln.ps_m = median(ln.ps, na.rm=T),
            ln.ps_d = mad(ln.ps, na.rm=T),
            sm.tt_m = median(sm.tt, na.rm=T),
            sm.tt_d = mad(sm.tt, na.rm=T)
  ) %>%
  as.data.frame()