#--created 2 sept 2025
#--calculating community value

library(tidyverse)
library(CENTSdata)

rm(list = ls())

# 1. species values (harm and benef) --------------------------------------------------------------

d1 <- 
  read_csv("data/tidy_spvalue.csv")

# 2. fall pct cover -------------------------------------------------------

d2 <- 
  cents_fallpctcover %>% 
  mutate(year = year(date2)) %>% 
  group_by(eu_id, year, eppo_code) %>% 
  summarise(cover_pct = mean(cover_pct)) 


# 3. combine --------------------------------------------------------------

d3 <- 
  d2 %>% 
  left_join(d1)

# 4. ecosys services (benef x pct cover) ----------------------------------

#--community pot by eu
d4 <- 
  d3 %>% 
  group_by(year, eu_id) %>% 
  summarise(potval = sum(pot_value*cover_pct/100))

d4 %>% 
  write_csv("data/tidy_communityvalue.csv")
