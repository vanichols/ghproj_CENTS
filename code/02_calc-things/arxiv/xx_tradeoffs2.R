#--comparing systems
#--created 16 june 2025
#--Make everythibng relative to no cc
#--keep it separate by years, can summarise for figure

library(tidyverse)
library(CENTSdata)

rm(list = ls())

# 1. species values (harm and benef) --------------------------------------------------------------

d1a <- 
  read_csv("data/tidy_spvalue.csv")

#--take max of pollin and econt---------------------------------------------

d1b <- 
  d1a %>%
  pivot_wider(names_from = cat, values_from = sc_value2_max) %>% 
  group_by(eppo_code) %>% 
  mutate(benef = max(ecocont, pollinator),
         net = benef - harm) 

d1b %>% 
  mutate(color = ifelse(net >= 0, "good", "bad")) %>% 
  ggplot(aes(eppo_code, net)) +
  geom_point(aes(color = color), size = 5)

d1 <- 
  d1b  %>% 
  select(eppo_code, harm, benef)


# 2. fall pct cover -------------------------------------------------------

d2 <- 
  cents_fallpctcover %>% 
  mutate(year = year(date2)) %>% 
  group_by(eu_id, year, eppo_code) %>% 
  summarise(cover_pct = mean(cover_pct)) %>% 
  left_join(d1)


# 3. biomass (no scaling) -----------------------------------------
#--sum within an eu (across subsamples), each year
d3a <- 
  cents_fallbio %>% 
  left_join(cents_eukey) %>% 
  mutate(year = year(date2)) %>% 
  group_by(eu_id, year, till_id, cctrt_id, straw_id) %>% 
  summarise(dm_gm2 = sum(dm_gm2, na.rm = T)) 

#--average over blocks

d3b <- 
  d3a %>%  
  group_by(year, till_id, cctrt_id, straw_id) %>% 
  summarise(value2 = mean(dm_gm2, na.rm = T)) 

d3 <- 
  d3b %>% 
  select(year, till_id, cctrt_id, straw_id, value2) %>% 
  mutate(name = "bio_gm2",
         cat = "service")
  


# 4. yields (no scaling) --------------------------------------------------

#--get average for each year/trt
d4a <- 
  cents_cropyields %>% 
  left_join(cents_eukey) %>% 
  mutate(year = year(date2)) %>% 
  group_by(year, till_id, cctrt_id, straw_id) %>% 
  summarise(value2 = mean(yield_dry_Mgha, na.rm = T))

d4 <- 
  d4a %>% 
  select(till_id, cctrt_id, straw_id, value2) %>% 
  mutate(name = "yield_dry_Mgha",
         cat = "service")



# 5. ecosys services (benef x pct cover) ----------------------------------

#--ecosercies by eu
d5a <- 
  d2 %>% 
  group_by(year, eu_id) %>% 
  summarise(benef = sum(benef*cover_pct/100))

d5a %>% 
  ggplot(aes(eu_id, benef)) +
  geom_point()

#--mean for each system, each year
d5b <- 
  d5a %>% 
  left_join(cents_eukey) %>% 
  group_by(year, till_id, straw_id, cctrt_id) %>% 
  summarise(value2 = mean(benef, na.rm = T)) %>% 
  ungroup() 

d5 <- 
  d5b %>% 
  select(year, till_id, straw_id, cctrt_id, value2) %>% 
  mutate(name = "benef",
         cat = "service")

# 6. PLI ------------------------------------------------------------------

d6a <- 
  read_csv("data/tidy_pesticide-load-by-eu.csv") %>% 
  left_join(cents_eukey) %>% 
  group_by(year, till_id, straw_id, cctrt_id) %>% 
  summarise(value2 = mean(load_ha, na.rm = T)) %>% 
  ungroup()

d6 <- 
  d6a %>%
  select(year, till_id, straw_id, cctrt_id, value2) %>% 
  mutate(name = "load_ha",
         cat = "dis-service")

# 7. agron harm, rel scale same as benef-----------------------------------------------------------

d7a <- 
  d2 %>% 
  group_by(year, eu_id) %>% 
  summarise(harm = sum(harm*cover_pct/100))

d7b <- 
  d7a %>% 
  left_join(cents_eukey) %>% 
  group_by(year, till_id, straw_id, cctrt_id) %>% 
  summarise(value2 = mean(harm, na.rm = T))

d7 <- 
  d7b %>% 
  select(year, till_id, straw_id, cctrt_id, value2) %>% 
  mutate(name = "harm",
         cat = "dis-service")


# 8. perenn weed legacy ---------------------------------------------------------------

#--total cirar and equar counted (perenn weeds)
leg <- 
  cents_spweedcount %>% 
  filter(weed_type %in% c("cirar", "equar")) %>% 
  group_by(eu_id) %>% 
  summarise(count = sum(count))

#--sum within a trt id
d8a <- 
  cents_spweedcount %>% 
  filter(weed_type == "cirar"|weed_type == "equar") %>% 
  group_by(eu_id, date2) %>% 
  summarise(count = sum(count))

#--average over blocks
d8b <- 
  d8a %>% 
  left_join(cents_eukey) %>% 
  mutate(year = year(date2)) %>% 
  group_by(year, till_id, straw_id, cctrt_id) %>% 
  summarise(value2 = mean(count, na.rm = T))

d8 <- 
  d8b %>% 
  select(year, till_id, straw_id, cctrt_id, value2) %>% 
  mutate(name = "pweed_nu",
         cat = "dis-service",
         year = year -1) #--because msmt happened following year


# 9. combine -------------------------------------------------------

d9 <- 
  d3 %>% 
  bind_rows(d4) %>% 
  bind_rows(d5) %>% 
  bind_rows(d6) %>% 
  bind_rows(d7) %>% 
  bind_rows(d8)

d9


# check -------------------------------------------------------------------

#--should have 6 cats
d9 %>% 
  unite(till_id, cctrt_id, straw_id, col = "sys") %>% 
  ggplot(aes(sys, value2)) +
  geom_point(aes(color = as.factor(year))) +
  facet_wrap(~name, scale = "free")

d9 %>% 
  filter(name == "harm")

# write -------------------------------------------------------------------

d9 %>% 
  write_csv("data/tidy_tradeoffs.csv")

