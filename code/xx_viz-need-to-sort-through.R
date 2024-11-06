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
pct <- read_csv("data/tidy/td_pctcover-simple.csv")


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


#--provide an example plot where this is happening

#--in 2019, instances where % cover is 0, but there is weed biomass

d1 %>% 
  left_join(d2) %>% 
  filter(!(comp_id %in% c("soil", "covercrop"))) %>% 
  filter(year == 2019)
  

# re-create bo's figs -----------------------------------------------------

p1 <- 
  pct %>% 
  mutate(cover_type2 = ifelse(cover_type %in% c("clover", "lolpe", "radish"), "covercrop", cover_type)) %>% 
  left_join(plot_key) %>% 
  mutate(yearf = as.factor(year))

library(lme4)
library(lmerTest)

p1

#--do any of these things impact the cover crop coverage?
m1 <- lmer(cover_pct ~ yearf*straw*cctrt_id*till_id + (1|block), data = p1 %>% filter(cover_type2 == "covercrop"))
anova(m1)
#--the cover crop type and year

p1 %>% 
  filter(cover_type2 == "covercrop") %>% 
  ggplot(aes(till_id, cover_pct)) + 
  geom_col(aes(fill = straw), position = position_dodge()) + 
  facet_grid(cctrt_id~ yearf)

#--there is a lot of cover crop coverage in the no-cover control....
p1 %>% 
  filter(cctrt_id == "nocc") %>% 
  ggplot(aes(cover_type, cover_pct)) + 
  geom_point() + 
  facet_grid(.~cctrt_id + yearf)

#--

p1 %>% 
  group_by(plot_id, year, cover_type2, block, straw, till_id, cctrt_id,
           yearf) %>% 
  summarise(cover_pct = sum(cover_pct, na.rm = T)) %>% 
  filter(cover_type2 == "covercrop",
         cctrt_id == "mix_E",
         year == 2018,
         straw == "retained"
         ) %>% 
  left_join(till_key) %>%
  mutate(instance_id = paste(year, block, plot_id)) %>%
  ggplot(aes(Bo_till, cover_pct)) +
  geom_point(aes(shape = straw, color = as.factor(block)))+
  facet_grid(straw~year)


# best plot ---------------------------------------------------------------

# I think it is important to classify the years, dry, wet, whatever

p1 %>% 
  group_by(year, cover_type2, block, straw, till_id, cctrt_id,
           yearf, rep) %>% 
  summarise(cover_pct = sum(cover_pct, na.rm = T)) %>%
  group_by(year, cover_type2, block, straw, till_id, cctrt_id,
           yearf) %>% 
  summarise(cover_pct = mean(cover_pct, na.rm = T)/100) %>% 
  filter(cover_type2 == "covercrop",
         #cctrt_id == "mix_E",
         #straw == "removed"
  ) %>% 
  left_join(till_key) %>%
  mutate(cctrt_id = factor(cctrt_id, levels = c("nocc", "mix_E", "mix_M",
                                                "rad_M", "rad_L"))) %>% 
  ggplot(aes(till_intensity, cover_pct)) +
  geom_line(aes(color = straw, group = interaction(block, straw, cctrt_id)), size = 2)+
  scale_color_manual(values = c("brown4", "orange")) +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_y_continuous(labels = label_percent()) +
  facet_grid(year ~ cctrt_id) +
  labs(title = "Radish performance less sensitive to weather, tillage, and timing compared to mixture",
       subtitle = "Both CCs benefited from earlier planting, radish less so than mix\nStraw removal and tillage had inconsistent, minimal effects",
       x = "Tillage intensity",
       y = "Catch crop cover (%)")

ggsave("figs/catch-crop-cover.png", width = 8, height = 6)

# scratch plots -----------------------------------------------------------


# average over blocks, I like the other one better
p1 %>% 
  group_by(year, cover_type2, block, straw, till_id, cctrt_id,
           yearf, rep) %>% 
  summarise(cover_pct = sum(cover_pct, na.rm = T)) %>%
  group_by(year, cover_type2, block, straw, till_id, cctrt_id,
           yearf) %>% 
  summarise(cover_pct = mean(cover_pct, na.rm = T)) %>% 
  group_by(year, cover_type2, straw, till_id, cctrt_id,
           yearf) %>% 
  summarise(cover_pct = mean(cover_pct, na.rm = T)) %>% 
  filter(cover_type2 == "covercrop",
         #cctrt_id == "mix_E",
         #year == 2018,
         #straw == "removed"
  ) %>% 
  left_join(till_key) %>%
 # mutate(cctrt_id = factor(cctrt_id, levels = c("nocc", "mix_E", "mix_M",
 #                                               "rad_M", "rad_L"))) %>% 
  ggplot(aes(till_intensity, cover_pct)) +
  geom_line(aes(color = straw, group = interaction(straw, year, cctrt_id)))+
  facet_grid(year ~ cctrt_id)


# should I keep all the observations
p1 %>% 
  group_by(year, cover_type2, block, straw, till_id, cctrt_id,
           yearf, rep) %>% 
  summarise(cover_pct = sum(cover_pct, na.rm = T)) %>%
  filter(cover_type2 == "covercrop",
         #cctrt_id == "mix_E",
         #straw == "removed"
  ) %>% 
  left_join(till_key) %>%
  mutate(cctrt_id = factor(cctrt_id, levels = c("nocc", "mix_E", "mix_M",
                                                "rad_M", "rad_L"))) %>% 
  ggplot(aes(till_intensity, cover_pct)) +
  geom_line(aes(color = straw, group = interaction(block, straw, rep, cctrt_id)))+
  facet_grid(year ~ cctrt_id)
