#--note: need to redo biomass stats in new way first

library(tidyverse)
library(CENTSdata)
library(patchwork)
library(ggh4x)

rm(list = ls())

source("code/00_color-palettes.R")

bl1 <- "#8e8eda"
bl2 <- "#424275"

th1 <-   theme(axis.ticks.y = element_blank(),
               axis.text.x = element_text(angle = 45, hjust = 1),
               legend.position = "top",
               strip.background.x = element_rect(fill = "white", 
                                                 color = "black"),
               strip.text.x = element_text(size = rel(1.3)),
               panel.border = element_blank())

# 1. data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
b_stats <- read_csv("data/stats_letters/letters_totbio-by-year.csv")

d1 <- as_tibble(cents_fallbio)


# 2. biomass plot (d)----------------------------------------------------------

#--regroup
d2 <- 
  d1 %>% 
  mutate(dm_gm2 = ifelse(is.na(dm_gm2), 0, dm_gm2)) %>% 
  mutate(dm_cat = case_when(
    dm_type == "grass_cl" ~ "covercrop",
    dm_type == "radish" ~ "covercrop",
    TRUE ~ "other"
  ),
  year = year(date2)) %>% 
  group_by(eu_id, year, dm_cat) %>% 
  summarise(dm_gm2 = sum(dm_gm2))
  
#--link to trts
d3 <- 
  d2 %>% 
  left_join(eu) %>% 
  distinct()

#--make nice cctrt
d4 <- 
  d3 %>% 
  mutate(cctrt_nice = case_when(
           cctrt_id == "nocc" ~ "NoCC",
           cctrt_id == "mix_E" ~ "MixE",
           cctrt_id == "mix_M" ~ "MixM",
           cctrt_id == "rad_M" ~ "RadM",
           cctrt_id == "rad_L" ~ "RadL",
           TRUE~"XXX"
         ),
         cctrt_id = factor(cctrt_id, levels = ord.cctrt_id),
         cctrt_nice = factor(cctrt_nice, levels = ord.cctrt_niceS))

#--make nice till
d5 <- 
  d4 %>% 
  mutate(
    till_id = factor(till_id, levels = ord.till_id),
         till_nice = case_when(
           till_id == "notill" ~ "No-till",
           till_id == "inversion" ~ "Inv",
           till_id == "surface" ~ "Surf",
           TRUE ~ "XXX"
         ),
         till_nice = factor(till_nice, levels = ord.till_nice))

#--make nice other stuff
d6 <- 
  d5 %>% 
  mutate(dm_cat = ifelse(dm_cat == "covercrop", "Cover crop", "Other"),
         dm_cat = factor(dm_cat, levels = c("Other", "Cover crop"))) 

#--group
d7a <-
  d6 %>%  
  group_by(cctrt_id, till_id, year,
           cctrt_nice, till_nice) %>% 
  summarise(dm_gm2 = mean(dm_gm2)) 

#--keep dm cat
d7b <-
  d6 %>%  
  group_by(dm_cat,
           cctrt_id, till_id, year,
           cctrt_nice, till_nice) %>% 
  summarise(dm_gm2 = mean(dm_gm2)) 

#--merge w stats
d8a <- 
  d7a %>% 
  left_join(b_stats, relationship = "many-to-many") %>% 
  mutate(letters = as.character(.group),
         letters = ifelse(year == 2019, str_to_lower(letters), letters))

d8b <- 
  d7b %>% 
  left_join(b_stats, relationship = "many-to-many")
  

p1 <-
  ggplot() +
  geom_col(data = d8b, 
           aes(till_nice, dm_gm2*0.01, fill = dm_cat),
           color = "black") +
  geom_linerange(data = d8a, 
                aes(x = till_nice, 
                    ymin = conf.low*0.01,
                    ymax = conf.high*0.01),
                color = "white") +
  geom_text(data = d8a, aes(x = till_nice, 
                             y = estimate*0.01 + 0.5, 
                             label = letters),
            size = 2) +
  facet_nested(.  ~ year + cctrt_nice) +
  scale_fill_manual(values = c("Cover crop" = bv3, 
                               "Other" = bl2)) +
  labs(x = NULL,
       y = "Fall biomass<br>(Mg ha<sup>-1</sup>)",
       fill = NULL) +
  theme_bw() +
  th1 +
  theme(axis.title.y = element_markdown())

p1

# 3. percent cover plot (a)--------------------------------------------------------



#--data
a1 <- as_tibble(cents_fallpctcover)

#--note this is still separated by species
a2 <- 
  eu %>% 
  left_join(a1) %>% 
  mutate(year = year(date2))


#--labels
a3 <- 
  a2 %>% 
  mutate(cover_frac = cover_pct / 100,
         cctrt_nice = case_when(
           cctrt_id == "nocc" ~ "NoCC",
           cctrt_id == "mix_E" ~ "MixE",
           cctrt_id == "mix_M" ~ "MixM",
           cctrt_id == "rad_M" ~ "RadM",
           cctrt_id == "rad_L" ~ "RadL",
           TRUE~"XXX"
         ),
         cctrt_id = factor(cctrt_id, levels = ord.cctrt_id),
         cctrt_nice = factor(cctrt_nice, levels = ord.cctrt_niceS),
         till_id = factor(till_id, levels = ord.till_id),
         cover_cat2 = case_when(
           cover_cat2 == "soil" ~ "Soil",
           cover_cat2 == "weed" ~ "Weed",
           cover_cat2 == "covercrop" ~ "Cover crop",
           TRUE ~ "Volunteer"
         ),
         cover_cat2 = factor(cover_cat2, levels = ord.cover_cat2),
         till_nice = case_when(
           till_id == "notill" ~ "No-till",
           till_id == "inversion" ~ "Inv",
           till_id == "surface" ~ "Surf",
           TRUE ~ "XXX"
         ),
         till_nice = factor(till_nice, levels = ord.till_nice)) 

#--sum across species
a4 <- 
  a3 %>%  
  group_by(cctrt_id, till_nice, cover_cat2,
           cctrt_nice, straw_id, block_id, eu_id, year, subrep) %>% 
  summarise(cover_pct = sum(cover_pct))

#--average across straws and blocks
a5 <- 
  a4 %>% 
  group_by(year, 
           cctrt_nice, 
           till_nice, 
           cover_cat2) %>% 
  summarise(cover_pct = mean(cover_pct, na.rm = T)) %>% 
  mutate(cover_cat2 = fct_rev(cover_cat2)) 

bl1 <- "#A0A0BA"
bl2 <- "#424275"
bl3 <- "#272746"

p2 <-
  a5 %>% 
  ggplot(aes(till_nice, cover_pct/100)) +
  geom_col(aes(fill = cover_cat2), color = "black") +
  facet_nested(.  ~ year + cctrt_nice) +
  scale_fill_manual(values = c("Soil" = bv1,
                               "Cover crop" = bv3,
                               "Volunteer" = bl1,
                               "Weed" = bl3),
                    guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  th1 +  
  theme(legend.position = "bottom") +
  labs(y = "Fall ground cover\n(%)",
       x = NULL,
       fill = NULL) +
  scale_y_continuous(labels = label_percent(),
                     breaks = c(0, .25, 0.5, .75, 1)) 

p2

p1 / p2

ggsave("figs/fig2_bio-and-cover.png", 
       width = 10, height = 8)
