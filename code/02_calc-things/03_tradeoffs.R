#--calculate community values ysing pct cover as a weight factor
#--updated 7 april 2025 to new tillage cats

library(tidyverse)
library(CENTSdata)

rm(list = ls())

# 1. species values (harm and benef) --------------------------------------------------------------

d1 <- 
  read_csv("data/tidy_spvalue.csv") %>% 
  select(eppo_code, harm, benef)


# 2. fall pct cover -------------------------------------------------------

d2 <- 
  cents_fallpctcover %>% 
  mutate(year = year(date2)) %>% 
  group_by(eu_id, year, eppo_code) %>% 
  summarise(cover_pct = sum(cover_pct)) %>% 
  left_join(d1)


# 3. biomass, summed across years and scaled across years -----------------------------------------
#--sum within an eu
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
  summarise(dm_gm2 = mean(dm_gm2, na.rm = T)) 

#--sum across years
d3c <- 
  d3b %>% 
  group_by(till_id, cctrt_id, straw_id) %>% 
  summarise(dm_gm2 = sum(dm_gm2, na.rm = T)) 

d3d <- 
  d3c %>% 
  ungroup() %>% 
  #not sure how to scale from 0-1, does this make sense
  mutate(mxval = max(dm_gm2),
         mnval = min(dm_gm2),
         value2 = (dm_gm2 - mnval)/(mxval - mnval))

d3d %>% 
  filter(dm_gm2 == mnval)

d3 <- 
  d3d %>% 
  select(till_id, cctrt_id, straw_id, value2) %>% 
  mutate(name = "dm_gm2",
         cat = "service")
  

# 4. yields, total across years, scaled across years, 0-1 range, 0 = no yields ------------------------------------------
#--changed! now scale within exp

#--get average for each year/trt
d4a <- 
  cents_cropyields %>% 
  left_join(cents_eukey) %>% 
  mutate(year = year(date2)) %>% 
  group_by(year, till_id, cctrt_id, straw_id) %>% 
  summarise(yield = mean(yield_dry_Mgha, na.rm = T))

#--sum over years
d4b <- 
  d4a %>% 
  group_by(till_id, cctrt_id, straw_id) %>% 
  summarise(yield = sum(yield, na.rm = T)) %>% 
  ungroup() 

d4c <-
  d4b %>% 
  mutate(mxval = max(yield),
         minval = min(yield),
         value2 = (yield - minval)/(mxval-minval)) %>% 
  ungroup()

d4c %>% 
  filter(yield == max(yield))

d4c %>% 
  filter(value2 == min(value2))

#--test, seems to work
#min 4, max 8, val  = x
x <- 6
(x-4)/(8 - 4)



d4 <- 
  d4c %>% 
  select(till_id, cctrt_id, straw_id, value2) %>% 
  mutate(name = "yield_dry_Mgha",
         cat = "service")


# 5. eco-services weighted by pct cover, 0-1 1 = max benefit observed -------------------------------------------------------------
#--mean over years, then take max value
#--ecosercies by eu
d5a <- 
  d2 %>% 
  group_by(year, eu_id) %>% 
  summarise(benef = sum(benef*cover_pct/100))

#--the max should be 3, if the community was 100% max benef
#--actual max value is only 0.7
#--scale within the experiment? Or on an absolute scale?
#--decided to scale on relative scale
d5b <- 
  d5a %>% 
  left_join(cents_eukey) %>% 
  group_by(till_id, straw_id, cctrt_id) %>% 
  summarise(benef = mean(benef, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(mxval = max(benef),
         value2 = benef/mxval)

#--the max value is 0.25 out of 3 if we use an abs scale 
d5b %>% 
  filter(benef == max(benef))


d5 <- 
  d5b %>% 
  select(till_id, straw_id, cctrt_id, value2) %>% 
  mutate(name = "benef",
         cat = "service")

d5 %>% 
  filter(value2 == 1)

# 6. PLI (relative sc.) ------------------------------------------------------------------

#--sum PLI over years
d6a <- 
  read_csv("data/tidy_pesticide-load-by-eu.csv") %>% 
  group_by(eu_id) %>% 
  summarise(load_ha = sum(load_ha))

#--mean, same as each eu value (I checked)
d6b <- 
  d6a %>% 
  left_join(cents_eukey) %>% 
  group_by(till_id, straw_id, cctrt_id) %>% 
  summarise(load_ha = mean(load_ha, na.rm = T)) %>% 
  ungroup()

d6c <-
  d6b %>% 
  mutate(mxval = max(load_ha),
         mnval = min(load_ha),
         value2 = (load_ha - mnval)/(mxval - mnval))

d6 <- 
  d6c %>%
  select(till_id, straw_id, cctrt_id, value2) %>% 
  mutate(name = "load_ha",
         cat = "dis-service")

d6c %>% 
  filter(load_ha == max(load_ha))

d6c %>% 
  filter(load_ha == min(load_ha))


# 7. agron harm, rel scale same as benef-----------------------------------------------------------

d7a <- 
  d2 %>% 
  group_by(year, eu_id) %>% 
  summarise(harm = sum(harm*cover_pct/100))

d7b <- 
  d7a %>% 
  left_join(cents_eukey) %>% 
  group_by(till_id, straw_id, cctrt_id) %>% 
  summarise(harm = mean(harm, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(mxval = max(harm),
         value2 = harm/mxval)

d7 <- 
  d7b %>% 
  select(till_id, straw_id, cctrt_id, value2) %>% 
  mutate(name = "harm",
         cat = "dis-service")


# 8. perenn weed legacy ---------------------------------------------------------------

#--total cirar counted in both years (should include both perenn weeds?)
leg <- 
  cents_spweedcount %>% 
  filter(weed_type == "cirar") %>% 
  group_by(eu_id) %>% 
  summarise(count = sum(count))


#--total cirar counted in both years (should include both perenn weeds?)
leg2 <- 
  cents_spweedcount %>% 
  filter(weed_type == "equar") %>% 
  group_by(eu_id) %>% 
  summarise(count2 = sum(count))

leg %>% 
  left_join(leg2) %>%
  left_join(cents_eukey) %>% 
  pivot_longer(count:count2) %>% 
  ggplot(aes(cctrt_id, value)) +
  geom_col(aes(fill = name)) +
  facet_grid(till_id ~ straw_id)

#yeah, include both
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
  summarise(count = mean(count, na.rm = T))

#--sum over years
d8c <- 
  d8b %>% 
  group_by(till_id, straw_id, cctrt_id) %>% 
  summarise(count = sum(count, na.rm = T))

#--scale within exp
d8d <- 
  d8c %>% 
  ungroup() %>% 
  mutate(mxval = max(count),
         value2 = count/max(count))

d8 <- 
  d8d %>% 
  select(till_id, straw_id, cctrt_id, value2) %>% 
  mutate(name = "count",
         cat = "dis-service")

d8d %>% 
  filter(count == max(count))

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

#--someone should have a value of 1 for everyhting
#--should have 6 cats
d9 %>% 
  filter(value2 == 1) %>% 
  pull(name) %>% 
  unique()

# write -------------------------------------------------------------------

d9 %>% 
  write_csv("data/tidy_tradeoffs.csv")

