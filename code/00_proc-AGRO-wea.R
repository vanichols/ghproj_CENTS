# created 8/4/2024
# purpose: process weather from AGRO weather station
# notes:


library(tidyverse)
library(lubridate)

rm(list = ls())

# get data from
# https://agro-web11t.uni.au.dk/klimadb/

# agro climate data -------------------------------------------------------

d1 <- 
  read_csv("data/raw/wea/1990-2013 AGRO-slagelse-US.csv") %>% 
  rename(prec = prec08) %>% 
  mutate(date = ymd(date),
         doy = yday(date),
         station = 613500) 

tst <- 
  d1 %>% 
  mutate(minte2 = as.numeric(minte))

tst2 <- 
  tst %>% 
  filter(is.na(minte2))

#--are there more NAs in a given year?
#--hmmm maybe this data isn't worth it
#--could take averages of every other year and fill them in I guess
tst2 %>% 
  mutate(year = year(date)) %>% 
  ggplot(aes(year)) + 
  geom_histogram()

d1a <- 
  d1 %>% 
  mutate_if(is.character, as.numeric) 

d1b <- 
  d1a %>%
  group_by(doy) %>% 
  mutate(minte_avg = mean(minte, na.rm = T),
         maxte_avg = mean(maxte, na.rm = T),
         temp_avg = mean(temp, na.rm = T),
         prec_avg = mean(prec, na.rm = T)) %>% 
  pivot_longer(minte:prec) %>% 
  mutate(value = case_when(
    (is.na(value) & name == "minte") ~ minte_avg,
    (is.na(value) & name == "maxte") ~ maxte_avg,
    (is.na(value) & name == "temp") ~ temp_avg,
    (is.na(value) & name == "prec") ~ prec_avg,
    TRUE ~ value
  )) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  select(station, date, doy, minte:prec)

#--it worked...STOPPED
d1b %>% 
  filter(is.na(minte))

d2 <- 
  read_csv("data/raw/wea/AGRO-slagelse-US.csv") %>% 
  mutate(date = dmy(date),
         doy = yday(date))

summary(d2)
#--10 years, need more for long term but can write code

d <- 
  d1b %>% 
  bind_rows(d2) %>% 
  rename(avgte = temp, prec_mm = prec)

# wea long term -----------------------------------------------------------

#####---temperature----#####

airtempLT <- 
  d %>% 
  mutate(doy = yday(date)) %>% 
  group_by(doy) %>% 
  summarise(LTavgte = mean(avgte))


####----cumulative precip----####

precLT <- 
  d %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  mutate(cprec_mm = cumsum(prec_mm)) %>% 
  group_by(doy) %>% 
  summarise(LTcprec_mm = mean(cprec_mm))

precLT

d_final <- 
  d %>% 
  left_join(airtempLT) %>% 
  left_join(precLT) %>% 
  select(-station) %>% 
  mutate(year = year(date)) %>% 
  select(date, year, doy, everything())


d_final %>%
  ungroup() %>% 
  write_csv("data/tidy/td_wea.csv")
