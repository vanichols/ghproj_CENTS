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

# 1. species ecovalue --------------------------------------------------------------

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

#--not much value in fall vegetation...is it even worth scaling?
d1d %>% 
  ggplot(aes(fall_ecovalue)) +
  geom_histogram()

#--sum all plant values
d1c <- 
  d1d %>% 
  group_by(eu_id, year) %>%
  summarise(fall_ecovalue = sum(fall_ecovalue))

#--average over years

d1d <- 
  d1c %>%
  group_by(eu_id) %>% 
  summarise(fall_ecovalue = mean(fall_ecovalue)) %>% 
  mutate(fall_ecovalue = fall_ecovalue/max(fall_ecovalue))

d1d %>% 
  ggplot(aes(fall_ecovalue)) +
  geom_histogram()

d1 <- 
  d1d %>% 
  select(eu_id, fall_ecovalue)

# 2. biomass -----------------------------------------

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
  summarise(dm_gm2 = mean(dm_gm2))

#--scale from 0-1

d2c <- 
  d2b %>%
  ungroup() %>% 
  mutate(fall_bio = dm_gm2/max(dm_gm2))

#--much better range  
d2c %>% 
  ggplot(aes(fall_bio)) +
  geom_histogram()

d2 <- 
  d2c %>% 
  select(eu_id, fall_bio)

# 3. grain yields --------------------------------------------------

d3a <- 
  cents_cropyields %>% 
  left_join(cents_eukey) %>% 
  mutate(year = year(date2)) %>% 
  group_by(eu_id, year) %>% 
  summarise(grain = mean(yield_dry_Mgha, na.rm = T))

#--average over years


#--scale within a year?
d3 <- 
  d3a %>% 
  group_by(year) %>% 
  mutate(grain = grain/max(grain))

d3 %>% 
  ggplot(aes(grain)) +
  geom_histogram()

d3

# 4. PLI ------------------------------------------------------------------

d4a <- 
  read_csv("data/tidy_pesticide-load-by-eu.csv") %>% 
  left_join(cents_eukey) %>% 
  group_by(eu_id, year) %>% 
  summarise(value2 = mean(load_ha, na.rm = T)) %>% 
  ungroup()

#--scale within a year
d4 <- 
  d4a %>% 
  group_by(year) %>% 
  mutate(pli = value2/max(value2)) %>% 
  select(-value2)

d4 %>% 
  ggplot(aes(pli)) +
  geom_histogram()

# 5. spring perennial weeds-----------------------------------------------------------

#--total cirar and equar counted (perenn weeds)
d5a <- 
  cents_spweedcount %>% 
  filter(weed_type %in% c("cirar", "equar")) %>%
  mutate(year = year(date2)) %>% 
  group_by(eu_id, year) %>% 
  summarise(count = sum(count))

d5a %>% 
  ggplot(aes(count)) +
  geom_histogram()

#--scale within a year
d5b <- 
  d5a %>% 
  group_by(year) %>% 
  mutate(spweeds_count = count/max(count))

d5 <- 
  d5b %>% 
  select(eu_id, year, spweeds_count) %>% 
  mutate(year = year -1) #--because msmt happened following year


# 6. combine -------------------------------------------------------
d4 %>% 
  arrange(eu_id)

d6 <- 
  d1 %>%
  left_join(d2) %>% 
  left_join(d3) %>%
  left_join(d4) %>% 
  left_join(d5)
            

d6


# 7. average over blocks --------------------------------------------------

d7 <- 
  d6 %>% 
  pivot_longer(fall_ecovalue:ncol(.)) %>% 
  left_join(cents_eukey) %>% 
  group_by(year, till_id, cctrt_id, straw_id, name) %>% 
  summarise(value = mean(value)) %>% 
  mutate(cropsys = paste(till_id, cctrt_id, straw_id)) %>% 
  ungroup() #%>% 
#select(year, cropsys, name, value)

d7 %>% 
  ggplot(aes(cropsys, value)) +
  geom_point(aes(color = name)) +
  coord_flip() +
  facet_grid(year ~ cctrt_id)

# 8. use typology weightings ----------------------------------------------

d8a <- 
  d0 %>% 
  left_join(d7, relationship = "many-to-many")


d8b <- 
  d8a %>% 
  mutate(wgtd_value = value * weight/100) %>% 
  group_by(farmer_type, cropsys, year) %>% 
  summarise(valuetot = sum(wgtd_value)/5)
  
d8b %>% 
  group_by(farmer_type, year) %>% 
  mutate(maxv = max(valuetot),
         clr = ifelse(valuetot == maxv, "MAX", "normal")) %>% 
  separate(cropsys, into = c("till_id", "cctrt_id", "straw_id"), sep = " ",
           remove = F) %>% 
  ggplot(aes(straw_id, valuetot)) +
  geom_point(aes(color = clr)) +
  facet_nested(cctrt_id+ till_id ~ farmer_type + year) +
  coord_flip()

# write -------------------------------------------------------------------

d9 %>% 
  write_csv("data/tidy_tradeoffs.csv")

