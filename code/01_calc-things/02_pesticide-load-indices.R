# created 17 feb 2025
# calculate PLI for each system
# modified: 2 aug 2025 - reran with updated cents package, till_id now surface instead of noninversion
# 27 jan 2026 reran with updated faba bean roundup app to 2.5 L

rm(list = ls())

library(tidyverse)
library(readxl)
library(CENTSdata)

#--Agil is not Agil 100 EC, as it should be
h <- 
  cents_herbops |> 
  mutate(product = ifelse(product == "Agil", "Agil 100 EC", product))
  
pli <- cents_pliherbs

pli_tot <- 
  pli |> 
  filter(load_cat == "Total")

h_look <- 
  h |> 
  select(date2, product, amount, amount_units) |> 
  distinct()

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

p3_tot <- 
  p3 %>% 
  filter(load_cat == "Total") |> 
  select(date2, product, amount, unit_applied, load_ha) |> 
  distinct()



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

p3 |> 
  filter(is.na(load_cat))

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
  ungroup()

p5 %>% 
  pull(till_id) %>% 
  unique()

p6 <- 
  p5 %>% 
  left_join(cents_eukey, relationship = "many-to-many") %>% 
  select(eu_id, year, load_ha) %>% 
  arrange(eu_id, year)

p6 %>%
  write_csv("data/tidy_pesticide-load-by-eu.csv")

p7 <- 
  p6 %>% 
  left_join(cents_eukey) %>% 
  group_by(year, till_id, rot_id, straw_id, cctrt_id) %>% 
  summarise(load_ha = mean(load_ha)) 

p7 %>% 
  ggplot(aes(cctrt_id, load_ha)) +
  geom_col(aes(fill = till_id),
           position = position_dodge()) +
  facet_grid(year ~ ., scales = "free")

p7

m1 <- lm(load_ha ~ till_id + cctrt_id + as.factor(year), data = p7)

anova(m1)


p7 %>% 
  write_csv("data/mette_pli-by-system.csv")
