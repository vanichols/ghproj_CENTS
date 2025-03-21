#--first understand if biomass and pct cover are related

library(tidyverse)
library(CENTSdata)
library(patchwork)
library(ggh4x)

rm(list = ls())

source("code/00_color-palettes.R")

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_fallbio)
c <- 
  as_tibble(cents_cropyields) %>% 
  mutate(year = year(date2)) %>% 
  dplyr::select(year, crop)

w <- 
  read_csv("data/tidy_weaclass.csv")



# orders ------------------------------------------------------------------


#--desired cctrt_id order
ord.cctrt_id <- c("nocc", "mix_E", "mix_M", "rad_M", "rad_L")

#--desired cctrt_nice order
ord.cctrt_nice <- c("NoCC", "MixEarly", "MixMid", "RadMid", "RadLate")

#--desired till_id order
ord.till_id <- c("notill", "noninversion", "inversion")

#--desired till_nice order
ord.till_nice <- c("No-till", "Non-inv", "Inv")


# were biomass and cover correlated ---------------------------------------
#--get average coverage for each catgory by plot

d_pct <- 
  cents_fallpctcover %>%
  #--sum by category
  group_by(eu_id, date2, subrep, cover_cat) %>% 
  summarise(cover_pct = sum(cover_pct)) %>% 
  #--average over subreps
  group_by(eu_id, date2, cover_cat) %>% 
  summarise(cover_pct = mean(cover_pct)) %>% 
  mutate(year = year(date2),
         dm_cat = cover_cat) %>% 
  select(-date2)
  
#--get biomass into same format
d_bio <- 
  y %>% 
  mutate(dm_gm2 = ifelse(is.na(dm_gm2), 0, dm_gm2)) %>% 
  mutate(dm_cat = case_when(
    dm_type == "grass_cl" ~ "covercrop",
    dm_type == "radish" ~ "covercrop",
    TRUE ~ "other"
  ),
  year = year(date2)) %>% 
  group_by(eu_id, year, dm_cat) %>% 
  summarise(dm_gm2 = sum(dm_gm2))
  
#--not really linear, worth showing separately?
d_bio %>% 
  left_join(d_pct) %>% 
  ggplot(aes(dm_gm2, cover_pct)) +
  geom_point(aes(color = dm_cat)) 


# 1. biomass linked to trts ---------------------------------------------------------------------
d1 <- 
  d_bio %>% 
  left_join(eu) %>% 
  left_join(w) %>% 
  distinct()


# 2. make nice lables cc ------------------------------------------------

d2 <- 
  d1 %>% 
  mutate(cctrt_nice = case_when(
           cctrt_id == "nocc" ~ "NoCC",
           cctrt_id == "mix_E" ~ "MixEarly",
           cctrt_id == "mix_M" ~ "MixMid",
           cctrt_id == "rad_M" ~ "RadMid",
           cctrt_id == "rad_L" ~ "RadLate",
           TRUE~"XXX"
         ),
         cctrt_id = factor(cctrt_id, levels = ord.cctrt_id),
         cctrt_nice = factor(cctrt_nice, levels = ord.cctrt_nice))


# 3. make nice tillage ----------------------------------------------------

d3 <- 
  d2 %>% 
  mutate(
    till_id = factor(till_id, levels = ord.till_id),
         precip = str_to_title(precip),
         till_nice = case_when(
           till_id == "notill" ~ "No-till",
           till_id == "inversion" ~ "Inv",
           till_id == "noninversion" ~ "Non-inv",
           TRUE ~ "XXX"
         ),
         till_nice = factor(till_nice, levels = ord.till_nice))


d4 <- 
  d3 %>% 
  mutate(
    straw_thing = ifelse(straw_id == "retained", "+", "-"),
    wea_straw = paste0(precip, straw_thing)) %>% 
  unite(precip, straw_thing, col = "wea_straw", sep = " ", remove = F)

d5 <-
  d4 %>%  
  group_by(cctrt_id, dm_cat, wea_straw, till_id, precip, straw_id) %>% 
  summarise(dm_gm2 = mean(dm_gm2)) 

d5 %>% 
  ggplot(aes(till_id, dm_gm2)) +
  geom_col(aes(fill = dm_cat)) +
  facet_nested(precip + straw_id  ~ cctrt_id) +
  scale_fill_manual(values = c("gold", "darkblue"))



# other versions ----------------------------------------------------------


d1 %>%
  mutate(
    straw_thing = ifelse(straw_id == "retained", "+", "-"),
    wea_straw = paste0(precip, straw_thing)) %>% 
  group_by(cctrt_id, dm_cat, wea_straw, till_id, precip, straw_id) %>% 
  summarise(dm_gm2 = mean(dm_gm2)) %>% 
  ggplot(aes(cctrt_id, dm_gm2)) +
  geom_col(aes(fill = dm_cat)) +
  facet_grid(precip + straw_id  ~ till_id) +
  scale_fill_manual(values = c("gold", "darkblue"))

