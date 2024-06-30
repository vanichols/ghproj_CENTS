# created 26/3/2024
# purpose: clean/separate Bo's CENTs data
# notes:


library(tidyverse)
library(plotly)
library(ggplotlyExtra)

rm(list = ls())

source("code/00_viz-settings.R")

# raw data ----------------------------------------------------------------

plot_key <- read_csv("data/keys/key_plot.csv")
cc_key<- read_csv("data/keys/key_cctrt.csv")
till_key <- read_csv("data/keys/key_till.csv")

#--note: three reps in pct data
pct <- read_csv("data/tidy/td_weedcover.csv")

#--note: no reps in biomass data?
bio <- read_csv("data/tidy/td_fallbio.csv")


# are biomass and pct cover related? --------------------------------------

#--when were things sampled for the coverage?
summary(pct %>% 
  mutate_if(is.character, as.factor))
#--9 Nov 2018
#--1 Nov 2019

#--for the biomass?
summary(bio %>% 
          mutate_if(is.character, as.factor))
#--14 Nov 2018
#--13 Nov 2019

#--let's pick one plot and see if they line up

pct %>% 
  filter(plot_id == "3_05_02" & year == "2018") %>% 
  mutate(comp_id = case_when(
    (cover_type %in% c("clover", "lolpe", "radish")) ~ "covercrop", 
    cover_type == "weedcov" ~ "weeds", 
    TRUE ~ cover_type)) 
  

bio %>% 
  filter(plot_id == "3_05_02" & year == "2018")


# look at pct cover -------------------------------------------------------

pct %>% 
  left_join(plot_key) %>% 
  mutate(cover_type2 = ifelse(cover_type %in% c("clover", "lolpe", "radish"), "cc", cover_type)) %>% 
  ggplot(aes(plot_id, cover_pct)) + 
  geom_col(aes(fill = cover_type2)) + 
  facet_wrap(~year + cctrt_id, scales = "free", ncol = 5) + 
  scale_fill_manual(values = c("green4", "black", "purple", "red")) + 
  coord_flip()


# 1. pct procesing -----------------------------------------------------------

#--need to do something with the reps
#--average over them I suppose

d1 <- 
  pct %>% 
  #--change to generic cover crop delineation
  mutate(comp_id = case_when(
    (cover_type %in% c("clover", "lolpe", "radish")) ~ "covercrop", 
     cover_type == "weedcov" ~ "weeds", 
     TRUE ~ cover_type)) %>% 
  group_by(plot_id, year, comp_id) %>% 
  summarise(cover_pct_avg = mean(cover_pct, na.rm = T))

#--sampled 13 Nov

d2 <- 
  bio %>% 
  mutate(comp_id = case_when(
    (dm_type %in% c("grass_cl", "radish")) ~ "covercrop", 
    TRUE ~ dm_type)) %>% 
  select(-date, -date2)


#--volunteer issues
#--did they all happen in 2019? Yes. 

tst <- 
  d1 %>% 
  left_join(d2) %>% 
  filter(comp_id != "soil") %>% 
  mutate(dm_gm2 = replace_na(dm_gm2, 0)) %>% 
  filter(dm_gm2 < 1 & cover_pct_avg > 0) %>% 
  filter(comp_id == "volunteer")


d1 %>% 
  left_join(d2) %>% 
  filter(comp_id != "soil") %>% 
  mutate(dm_gm2 = replace_na(dm_gm2, 0)) %>% 
  ggplot(aes(dm_gm2, cover_pct_avg)) + 
  geom_point(aes(color = comp_id, shape = as.factor(year)), size = 3, show.legend = F, alpha = 0.5) +
  facet_grid(year~comp_id) +
  labs(x = "Dry matter from one plot sample (g/m2)",
       y = "Percent cover, average of three samples from plot (%)",
       title = str_wrap("No dry matter measurements of volunteers in 2019?"),
       subtitle = "Also in 2019, instances where weeds had no ground cover, but had biomass") + 
  th1_legend

##--weed biomass vs pct catch crop cover
d1 %>%
  filter(comp_id == "covercrop") %>% 
  select(plot_id, year, cccover_pct = cover_pct_avg) %>% 
  left_join(d2 %>% 
              filter(comp_id=="weeds") %>% 
              select(plot_id, year, dm_gm2_weeds = dm_gm2)) %>% 
  ggplot(aes(cccover_pct, dm_gm2_weeds)) +
  geom_point() +
  labs(title = "weed biomass not related to catch crop coverage")

##--there should be higher cc values than 30%
d1 %>%
  filter(comp_id == "covercrop") %>%
  left_join(plot_key) %>% 
  left_join(cc_key) %>% 
  filter(cctrt_id == "rad_M", year == 2018, till_id == "notill")
  select(plot_id, year, cccover_pct = cover_pct_avg)

##--weed biomass vs pct catch crop bio
d2 %>%
  filter(comp_id == "covercrop") %>% 
  select(plot_id, year, dm_gm2_cc = dm_gm2) %>% 
  left_join(d2 %>% 
              filter(comp_id=="weeds") %>% 
              select(plot_id, year, dm_gm2_weeds = dm_gm2)) %>% 
  ggplot(aes(dm_gm2_cc, dm_gm2_weeds)) +
  geom_point()
