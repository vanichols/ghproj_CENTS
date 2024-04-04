# created 26/3/2024
# purpose: clean/separate Bo's CENTs data
# notes:


library(tidyverse)
library(readxl)

rm(list = ls())



# raw data ----------------------------------------------------------------

draw <- read_csv("data/raw/rd_gina-simplified.csv")



# 1. yields ---------------------------------------------------------------

d1 <-
  draw %>% 
  filter(!is.na(yield_DM)) %>% 
  select(plot_id:date2, 
         yield_dry_gm2 = yield_DM, 
         yield_15p_tonha = yield15) %>% 
  mutate(crop = case_when(
    year == 2018 ~ "barley",
    year == 2019 ~ "oat",
    year == 2020 ~ "Not sure"))

d1 %>% 
  ggplot(aes(year, yield_15p_tonha)) + 
  geom_jitter(aes(color = crop), width = 0.2, size = 2)

d1 %>% 
  select(plot_id, year, crop, date, date2, everything()) %>% 
  write_csv("data/tidy/td_yield.csv")


# need to do percent coverage ---------------------------------------------


