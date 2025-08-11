#--calc one value for each system using mult farmer typologies
#--created 2 aug 2025
#--keep it separate by years? Then what to do with the faba bean yields?
#--no, average over years, then scale? What about blocks? Average over those too?
#--stopped without resolving the averaging question...

library(tidyverse)
library(CENTSdata)
library(readxl)
library(ggh4x)

rm(list = ls())

# 0. typology weights -----------------------------------------------------

d0 <- 
  read_excel("data/byhand_typology-weights.xlsx", skip = 5) %>% 
  pivot_longer(grain:pli) %>% 
  rename(weight = value)

# 1. species ecovalue GOOD--------------------------------------------------------------

d1a <- 
  read_csv("data/tidy_spvalue.csv")

#--just keep ecocont

d1b <- 
  d1a %>%
  filter(cat == "ecocont")

d1b %>% 
  ggplot(aes(x = reorder(eppo_code, sc_value2_max), 
             y = sc_value2_max)) +
  geom_point(aes(size = sc_value2_max)) +
  coord_flip()

d1c <- 
  d1b  %>% 
  select(eppo_code, fall_ecovalue = sc_value2_max)

#--weight pct cover by ecovalue

d1d <- 
  cents_fallpctcover %>% 
  mutate(year = year(date2)) %>% 
  group_by(eu_id, year, eppo_code) %>% 
  summarise(cover_pct = mean(cover_pct)) %>% 
  left_join(d1c) %>% 
  group_by(eu_id, year) %>% 
  mutate(fall_ecovalue = cover_pct/100 * fall_ecovalue)

#--sum all plant values within an eu and year
d1c <- 
  d1d %>% 
  group_by(eu_id, year) %>%
  summarise(fall_ecovalue = sum(fall_ecovalue))
d1c %>% 
  ggplot(aes(fall_ecovalue)) +
  geom_histogram()

#--average over years, don't scale yet
d1d <- 
  d1c %>%
  group_by(eu_id) %>% 
  summarise(fall_ecovalue = mean(fall_ecovalue)) 

d1d %>% 
  ggplot(aes(fall_ecovalue)) +
  geom_histogram()

d1 <- 
  d1d %>% 
  select(eu_id, fall_ecovalue)

# 2. biomass GOOD-----------------------------------------

#--sum within an eu (across subsamples), each year
d2a <- 
  cents_fallbio %>% 
  left_join(cents_eukey) %>% 
  mutate(year = year(date2)) %>% 
  group_by(eu_id, year) %>% 
  summarise(dm_gm2 = sum(dm_gm2, na.rm = T)) 

#--average over years
d2b <- 
  d2a %>% 
  group_by(eu_id) %>% 
  summarise(fall_bio = mean(dm_gm2))

d2 <- 
  d2b %>% 
  select(eu_id, fall_bio)

# 3. grain yields GOOD--------------------------------------------------

d3a <- 
  cents_cropyields %>% 
  left_join(cents_eukey) %>% 
  mutate(year = year(date2)) %>% 
  group_by(eu_id, year) %>% 
  summarise(grain = mean(yield_dry_Mgha, na.rm = T))

#--sum across years
d3 <- 
  d3a %>% 
  group_by(eu_id) %>% 
  summarise(grain = sum(grain))

d3 %>% 
  ggplot(aes(grain)) +
  geom_histogram()

d3

# 4. PLI BAD------------------------------------------------------------------

d4a <- 
  read_csv("data/tidy_pesticide-load-by-eu.csv") %>% 
  left_join(cents_eukey) %>% 
  group_by(eu_id, year) %>% 
  summarise(value2 = mean(load_ha, na.rm = T)) %>% 
  ungroup()

#--add across years
d4 <- 
  d4a %>% 
  group_by(eu_id) %>% 
  summarise(pli = sum(value2)) 

d4 %>% 
  ggplot(aes(pli)) +
  geom_histogram()

# 5. spring perennial weeds BAD-----------------------------------------------------------

#--total cirar and equar counted (perenn weeds)
#--sum across years for each eu
d5 <- 
  cents_spweedcount %>% 
  filter(weed_type %in% c("cirar", "equar")) %>%
  mutate(year = year(date2)) %>% 
  group_by(eu_id) %>% 
  summarise(spweeds_count = sum(count))

d5 %>% 
  ggplot(aes(spweeds_count)) +
  geom_histogram()


# 6. combine -------------------------------------------------------

d6 <- 
  d1 %>%
  left_join(d2) %>% 
  left_join(d3) %>%
  left_join(d4) %>% 
  left_join(d5)
            

d6


# 7. get average for each cropsys --------------------------------------------------

d7 <- 
  d6 %>% 
  pivot_longer(fall_ecovalue:ncol(.)) %>% 
  left_join(cents_eukey) %>% 
  group_by(till_id, cctrt_id, straw_id, name) %>% 
  summarise(value = mean(value)) %>% 
  mutate(cropsys = paste(till_id, cctrt_id, straw_id)) %>% 
  ungroup() #%>% 
#select(year, cropsys, name, value)


# 8. scale from 0-1 -------------------------------------------------------
#--scale everything on absolute terms

d8a <- 
  d7 %>% 
  group_by(name) %>% 
  mutate(mx = max(value),
            sc_value = value/mx)


d8a %>% 
  ggplot(aes(cropsys, sc_value)) +
  geom_point(aes(color = name)) +
  coord_flip() +
  facet_grid(.~name)

d8b <- 
  d8a %>% 
  select(cropsys, name, sc_value)

#--adjust for bad versus good outcomes
d8 <- 
  d8b %>% 
  mutate(sc_value2 = case_when(
    name == "pli" ~ 1-sc_value,
    name == "spweeds_count" ~ 1-sc_value,
    TRUE ~ sc_value)
  )
  
d8 %>% 
  filter(cropsys == "notill mix_E removed")

# 9. use typology weightings ----------------------------------------------

d9a <- 
  d8 %>% 
  left_join(d0, relationship = "many-to-many")


d9b <- 
  d9a %>% 
  mutate(wgtd_value = sc_value2 * weight/100) 

d9c <- 
  d9b %>% 
  group_by(farmer_type, cropsys) %>% 
  summarise(valuetot = sum(wgtd_value)) 


d9c %>% 
  filter(is.na(farmer_type))


d9c %>% 
  ggplot(aes(reorder(cropsys, valuetot), valuetot, fill = farmer_type)) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~farmer_type)

d9d <- 
  d9c %>% 
  group_by(farmer_type) %>% 
  slice_max(n = 3, order_by = valuetot)

d9d %>% 
  ggplot(aes(farmer_type, valuetot, fill = cropsys)) +
  geom_col(position = "dodge")

d9d <- 
  d9c %>% 
  group_by(farmer_type) %>% 
  slice_min(n = 3, order_by = valuetot)

d9d %>% 
  ggplot(aes(farmer_type, valuetot, fill = cropsys)) +
  geom_col(position = "dodge")

d9 <- 
  d9c 

# write -------------------------------------------------------------------

d9 %>% 
  write_csv("data/tidy_typology-values.csv")

