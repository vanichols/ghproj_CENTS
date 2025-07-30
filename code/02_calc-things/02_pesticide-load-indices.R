# created 17 feb 2025
# calculate PLI for each system

library(tidyverse)
library(readxl)
library(CENTSdata)



h <- cents_herbops
pli <- cents_pliherbs



# 1. combine --------------------------------------------------------------

p1 <- 
  h %>% 
  left_join(
    pli %>% 
      select(product, load_cat, load_unit, load_unit_units),
    relationship = "many-to-many"
  ) %>% 
  mutate(year = year(date2)) 



# 2.  ---------------------------------------------------------------------


p2 <- 
  p1 %>% 
  separate(amount_units, into = c("unit_applied", "land_unit_appliedto"), sep = "/") %>% 
  separate(load_unit_units, into = c("trash", "load_unit_lorg"), sep = "/") 

#--check to make sure things match up, this should be empty
p2 %>% 
  filter(unit_applied != load_unit_lorg)


# 3. calcs ----------------------------------------------------------------

p3 <- 
  p2 %>% 
  mutate(
    amount = as.numeric(amount),
    load_ha = amount * load_unit) 

#--vuslaize to check

p3 %>% 
  filter(load_cat == "Total") %>% 
  ggplot(aes(cctrt_id, load_ha)) + 
  geom_col(aes(fill = product), color = "black") +
  facet_grid(till_id ~ year)

p3 %>% 
  ggplot(aes(cctrt_id, load_ha)) + 
  geom_col(aes(fill = product)) +
  facet_grid(till_id ~ load_cat)

p4 <- 
  p3 %>% 
  group_by(year, till_id, cctrt_id, product, load_cat) %>% 
  summarise(load_ha = sum(load_ha)) %>% 
  #--agil has NAs bc it has no load
  filter(!is.na(load_ha))

p4 %>% 
  ggplot(aes(cctrt_id, load_ha)) + 
  geom_col(aes(fill = product)) +
  facet_grid(till_id ~ load_cat)

p4 %>% 
  filter(year != 2020) %>% 
  filter(load_cat == "Total") %>% 
  ggplot(aes(cctrt_id, load_ha)) + 
  geom_col(aes(fill = product), color = "black") +
  facet_grid(. ~ till_id)

p4 %>% 
  filter(year != 2020) %>% 
  filter(load_cat == "Total") %>% 
  group_by(till_id, cctrt_id) %>% 
  summarise(load_ha = sum(load_ha)) %>% 
  ggplot(aes(till_id, cctrt_id)) + 
  geom_point(aes(size = load_ha, color = load_ha)) +
  scale_color_viridis_c(option = "plasma") + 
  scale_size(range = c(1, 15))


# make it on an eu basis --------------------------------------------------

p5 <- 
  p4 %>% 
  filter(year != 2020) %>% 
  filter(load_cat == "Total") %>% 
  group_by(year, till_id, cctrt_id) %>% 
  summarise(load_ha = sum(load_ha)) %>% 
  left_join(cents_eukey, relationship = "many-to-many") %>% 
  ungroup() %>% 
  select(eu_id, year, load_ha) %>% 
  arrange(eu_id, year)

p5 %>%
  write_csv("data/tidy_pesticide-load-by-eu.csv")

p5 %>% 
  left_join(cents_eukey) %>% 
  group_by(year, till_id, rot_id, straw_id, cctrt_id) %>% 
  summarise(load_ha = mean(load_ha)) %>% 
  write_csv("data/mette_pli-by-system.csv")
