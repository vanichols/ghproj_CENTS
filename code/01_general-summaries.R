library(tidyverse)
library(CENTSdata)

#--number of gdds accumulated from planting to sampling biomass
d0 <- 
  cents_gdds %>% 
  left_join(cents_eukey) %>% 
  select(year = ccest_year, pl2samp_gdd, cctrt_id) %>% 
  distinct()

#--total cover crop biomass at sampling

d1 <- 
  cents_fallbio %>%
  left_join(cents_eukey) %>% 
  separate(cctrt_id, into = c("cc", "pl"), sep = "_", remove = F) %>% 
  mutate(dm_cat = case_when(
    (dm_type == "grass_cl" & cc == "mix") ~ "covercrop",
    (dm_type == "radish" & cc == "rad") ~ "covercrop",
    TRUE ~ "XX"),
    year = year(date2)) %>% 
  select(year, cctrt_id, dm_cat, dm_gm2) %>% 
  filter(dm_cat == "covercrop")


#--is gdd correlated with biomass?
d1 %>% 
  left_join(d0, relationship = "many-to-many") %>% 
  ggplot(aes(pl2samp_gdd, dm_gm2)) + 
  geom_point(aes(color = as.factor(year))) +
  facet_grid(.~cctrt_id)
