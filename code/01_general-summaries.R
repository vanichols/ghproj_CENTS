library(tidyverse)
library(CENTSdata)

#--number of gdds accumulated from planting to sampling biomass
cents_gdds %>% 
  left_join(cents_eukey) %>% 
  select(ccest_year, pl2samp_gdd, cctrt_id) %>% 
  distinct()

#--total cover crop biomass at sampling

d1 <- 
  cents_fallbio %>%
  left_join(cents_eukey) %>% 
  separate(cctrt_id, into = c("cc", "pl"), sep = "_", remove = F) %>% 
  mutate(dm_cat = case_when(
    (dm_type == "grass_cl" & cc == "mix") ~ "covercrop",
    (dm_type == "radish" & cc == "rad") ~ "covercrop",
    TRUE ~ "XX")) %>% 
  select(cctrt_id, cc, pl, dm_cat, dm_type, dm_gm2, everything()) %>% 
  filter(dm_cat == "covercrop")

#--only one sample per plot? looks like it
d1 %>% 
  filter(block_id == "b1") %>% 
  ggplot(aes(cctrt_id, dm_gm2)) +
  geom_point(aes(color = till_id)) +
  facet_grid(straw_id~date2)

#--I should fit a statistical model to get the means, but I won't right now
d1 %>% 
  mutate(year = lubridate::year(date2)) %>% 
  group_by(year, cctrt_id) %>% 
  summarise(dm_gm2 = mean(dm_gm2))
