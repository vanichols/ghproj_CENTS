# created 30/6/2024
# purpose: clean up hand-made planting dates of cover crops (maybe add other ops later)
# notes: est_year is the year the cc is established

library(tidyverse)
library(readxl)
library(lubridate)

Sys.setenv(LANG = "en")
rm(list = ls())


# raw data ----------------------------------------------------------------

d_praw <- read_excel("data/raw/byhand_cover-crop-planting-dates.xlsx",
                   sheet = "ccplanting", skip = 5) 

d_traw <- read_excel("data/raw/byhand_cover-crop-planting-dates.xlsx",
                   sheet = "ccterminating", skip = 5) 

d_tillraw <- read_csv("data/keys/key_till.csv")
d_ccraw <- read_csv("data/keys/key_cctrt.csv")

lst_till <- d_tillraw %>% select(till_id)
lst_cc <- d_ccraw %>% select(cctrt_id)

# process cc planting and termination-----------------------------------------------------------------

#--planting
d_p <- 
  d_praw %>% 
  mutate(date = paste(year, month, day, sep = "-"),
         date = as_date(date),
         doy = yday(date),
         year = year(date),
         est_year = as.numeric(est_year)) %>% 
  select(date, est_year, year, doy, cctrt_id)

d_p2 <- 
  expand_grid(lst_till, lst_cc) %>% 
  left_join(d_p, relationship = "many-to-many") %>% 
  mutate(activity = "cc_planting")

#--termination
d_t <- 
  d_traw %>% 
  mutate(date = paste(year, month, day, sep = "-"),
         date = as_date(date),
         doy = yday(date),
         year = year(date)) %>% 
  select(date, est_year, year, doy, till_id)

#--applied to all ccs within a tillage id equally
d_t2 <- 
  expand_grid(lst_till, lst_cc) %>% 
  left_join(d_t, relationship = "many-to-many") %>% 
  mutate(activity = "cc_termination") %>% 
  filter(cctrt_id != "nocc")


#--create dummy nocc to add
d_t3 <- 
  d_t2 %>% 
  bind_rows(lst_till %>% 
              mutate(cctrt_id = "nocc",
                     activity = "cc_termination")) %>% 
  arrange(till_id, cctrt_id)

d_final <- 
  d_p2 %>%
  bind_rows(d_t3)


d_final %>%
  write_csv("data/tidy/td_cc-plant-term-dates.csv")
