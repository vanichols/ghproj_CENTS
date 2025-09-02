#--created 2 sept 2025
#--join coverage and biomass figure?
#--adding GDDs for each cc in each year to figure

library(tidyverse)
library(CENTSdata)
library(patchwork)
library(ggh4x)
library(ggpattern)

rm(list = ls())

source("code/00_color-palettes.R")



# figure things -----------------------------------------------------------

w_clr <- "#d3d3dd"
wv_clr <- "#A0A0BA"
v_clr <-  "#7c7c99"
cc_clr <- bv3

#"#424275"
th1 <-   theme(axis.ticks.y = element_blank(),
               axis.text.x = element_text(angle = 45, hjust = 1),
               legend.position = "top",
               strip.background.x = element_rect(fill = "white", 
                                                 color = "black"),
               strip.text.x = element_text(size = rel(1.3)),
               panel.border = element_blank())

# 1. data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
b_stats <- read_csv("data/stats/letters/letters_totbio-by-year.csv")
d1 <- as_tibble(cents_fallbio)

gdds <- 
  cents_gdds %>% 
  rename(year = ccest_year) %>% 
  left_join(eu) %>% 
  filter(cctrt_id != "nocc") %>% 
    group_by(cctrt_id, year) %>% 
    summarise(pl2samp_gdd = mean(pl2samp_gdd)) %>% 
  mutate(till_id = "Surf",#--just to get it centered in facets
         pl2samp_gdd = as.character(round(pl2samp_gdd, -1))) %>% 
  ungroup() %>% 
  add_row(cctrt_id = "nocc", year = 2018, pl2samp_gdd = "NA", till_id = "surface") %>% 
  add_row(cctrt_id = "nocc", year = 2019, pl2samp_gdd = "NA", till_id = "surface") %>% 
  MakeNiceLabels(.) %>% 
  mutate(till_nice = 2)

# 2. biomass plot (d)----------------------------------------------------------

#--regroup
d2a <- 
  d1 %>% 
  mutate(dm_gm2 = ifelse(is.na(dm_gm2), 0, dm_gm2)) %>% 
  mutate(dm_cat = case_when(
    dm_type == "grass_cl" ~ "covercrop",
    dm_type == "radish" ~ "covercrop",
    dm_type == "volunteer" ~ "volunteer",
    dm_type == "weeds" ~ "weed",
    TRUE ~ "other"
  ),
  year = year(date2)) %>% 
  group_by(eu_id, year, dm_cat) %>% 
  summarise(dm_gm2 = sum(dm_gm2))
  

#--2019 had no volunteers, make new group

d2 <- 
  d2a %>% 
  mutate(dm_cat2 = case_when(
    (year == 2019 & dm_cat == "volunteer") ~ "weed and/or volunteer",
    (year == 2019 & dm_cat == "weed") ~ "weed and/or volunteer",
     TRUE ~ dm_cat)) %>% 
  group_by(eu_id, year, dm_cat2) %>% 
  summarise(dm_gm2 = sum(dm_gm2))

#--link to trts
d3 <- 
  d2 %>% 
  left_join(eu) %>% 
  distinct()

#--make nice cctrt and tilltrt
d5 <- 
  d3 %>% 
  MakeNiceLabels(.)

#--make nice other stuff
d6 <- 
  d5 %>% 
  mutate(dm_cat2 = ifelse(dm_cat2 == "covercrop", "Cover crop", dm_cat2),
         dm_cat2 = str_to_sentence(dm_cat2),
         dm_cat2 = fct_inorder(dm_cat2)) 

#--group
d7a <-
  d6 %>%  
  group_by(cctrt_id, till_id, year,
           cctrt_nice, till_nice) %>% 
  summarise(dm_gm2 = mean(dm_gm2)) 

#--keep dm cat
d7b <-
  d6 %>%  
  group_by(dm_cat2,
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
  geom_col_pattern(data = d8b, 
           aes(till_nice, 
               dm_gm2*0.01,
               fill= dm_cat2,
               pattern = dm_cat2),
           pattern_fill = w_clr,
           pattern_density = 0.5,        # Adjust pattern density
           pattern_spacing = 0.25,       # Adjust pattern spacing
           pattern_key_scale_factor=.1,
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
  geom_text(data = gdds, aes(x = till_nice, 
                            y = 4.5, 
                            label = pl2samp_gdd),
            size = 3, hjust = 0.5, fontface = "italic") +
  facet_nested(.  ~ year + cctrt_nice) +
  scale_fill_manual(values = c("Cover crop" = cc_clr,
                               "Weed and/or volunteer" = v_clr,
                               "Weed" = w_clr,
                               "Volunteer" = v_clr),
                            ) +
  scale_pattern_manual(values = c("Cover crop" = "none",
                                  "Weed and/or volunteer" = "stripe",
                                  "Weed" = "none",
                                  "Volunteer" = "none")) +
  labs(x = NULL,
       y = "Fall biomass<br>(Mg ha<sup>-1</sup>)",
       fill = NULL,
       pattern = NULL) +
  theme_bw() +
  th1 +
  theme(axis.title.y = element_markdown(),
        #legend.key.size = unit(1, 'cm')
        )



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


p2 <-
  a5 %>%
  mutate(cover_cat2 = factor(cover_cat2, 
                             levels = c("Soil", 
                                        "Cover crop", 
                                        "Volunteer",
                                        "Weed")),
         cover_cat2 = fct_rev(cover_cat2)) %>% 
  ggplot(aes(till_nice, cover_pct/100)) +
  geom_col(aes(fill = cover_cat2), color = "black") +
  facet_nested(.  ~ year + cctrt_nice) +
  scale_fill_manual(values = c("Soil" = bv1,
                               "Cover crop" = cc_clr,
                               "Volunteer" = v_clr,
                               "Weed" = w_clr),
                    guide = guide_legend(reverse = TRUE)
                    ) +
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
