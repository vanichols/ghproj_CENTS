#--I need to do the stats on the sp weed counts
#--seems like a big year effect, tillage effect, less cctrt effect
#--dicots impacted probably, maybe cirar

pli <- read_csv("data/tidy_pesticide-loads-per-system.csv")

pli %>% 
  filter(year != 2020) %>% 
  filter(load_cat == "Total") %>% 
  group_by(till_id, cctrt_id) %>% 
  summarise(load_ha = sum(load_ha)) %>% 
  ggplot(aes(till_id, cctrt_id)) + 
  geom_point(aes(size = load_ha, color = load_ha)) +
  scale_color_viridis_c(option = "plasma") + 
  scale_size(range = c(1, 15))

#--clear pattern
cents_spweedcount %>% 
  left_join(cents_eukey) %>% 
  mutate(year = year(date2)) %>% 
  ggplot(aes(year, count)) +
  geom_jitter(aes(color = weed_type)) + 
  facet_grid(weed_type~till_id, scales = "free")

#--no pattern
cents_spweedcount %>% 
  left_join(cents_eukey) %>% 
  ggplot(aes(till_id, count)) +
  geom_jitter(aes(color = weed_type)) + 
  facet_grid(.~cctrt_id)
