# created 19 dec 2024
# purpose: look at fall pct cover from a 'value' perspective

library(tidyverse)
library(lme4)
library(lmerTest)
library(CENTSdata)
library(broom)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(car)

rm(list = ls())

# data --------------------------------------------------------------------

w <- cents_wea
eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_fallpctcover)
w <- read_csv("data/tidy_weaclass.csv")

#--data separated by species
d_sp <- 
  eu %>% 
  left_join(y) %>% 
  mutate(year = year(date2)) %>% 
  left_join(w) %>% 
  mutate(weayear = paste(precip, te, sep  = "-"),
         cover_frac = cover_pct / 100)

#--species 'values'
d_v <- read_csv("data/tidy_spvalue.csv")

d_v %>% 
  #filter(harm > 1) %>% 
  ggplot(aes(harm, benef)) +
  geom_point(aes(color = eppo_code), size = 5) 

# 1. combine pctcover w spval ---------------------------------------------

d1 <- 
  d_sp %>% 
  left_join(d_v) %>%  
  mutate(wharm = harm * cover_frac,
         wben = benef * cover_frac,
         wnet = net * cover_frac) 

d1 %>% 
  ggplot(aes(cctrt_id, wnet)) +
  geom_point(aes(color = straw_id)) +
  facet_grid(weayear ~ till_id + straw_id)

d1 %>% 
  ggplot(aes(cctrt_id, wnet)) +
  geom_point(aes(color = straw_id))


d1 %>% 
  ggplot() +
  geom_point(aes(till_id, wben), color = "darkblue") +
  geom_point(aes(till_id, -wharm), color = "red2") +
  facet_grid(weayear ~ cctrt_id + straw_id)
