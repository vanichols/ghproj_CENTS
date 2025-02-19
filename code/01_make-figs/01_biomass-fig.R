#--first understand if biomass and pct cover are related

library(tidyverse)
library(CENTSdata)
library(patchwork)

rm(list = ls())

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_fallbio)
c <- 
  as_tibble(cents_cropyields) %>% 
  mutate(year = year(date2)) %>% 
  dplyr::select(year, crop)

w <- 
  read_csv("data/tidy_weaclass.csv")



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


# viz ---------------------------------------------------------------------
d1 <- 
  d_bio %>% 
  left_join(eu) %>% 
  left_join(w) %>% 
  distinct()


#--removing straw had lowest impact, and no interactions
d1 %>%
  mutate(
    straw_thing = ifelse(straw_id == "retained", "+", "-"),
    wea_straw = paste0(precip, straw_thing)) %>% 
  ggplot(aes(block_id, dm_gm2)) +
  geom_col(aes(fill = dm_cat)) +
  facet_grid(wea_straw + till_id  ~ cctrt_id) +
  scale_fill_manual(values = c("gold", "darkblue"))

#--I think showing the reps is too much for this figure
d1 %>%
  mutate(
    straw_thing = ifelse(straw_id == "retained", "+", "-"),
    wea_straw = paste0(precip, straw_thing)) %>% 
  group_by(cctrt_id, dm_cat, wea_straw, till_id, precip, straw_id) %>% 
  summarise(dm_gm2 = mean(dm_gm2)) %>% 
  ggplot(aes(till_id, dm_gm2)) +
  geom_col(aes(fill = dm_cat)) +
  facet_grid(precip + straw_id  ~ cctrt_id) +
  scale_fill_manual(values = c("gold", "darkblue"))

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
