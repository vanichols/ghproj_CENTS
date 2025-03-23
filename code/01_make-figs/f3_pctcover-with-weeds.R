# created 5 nov 2024
# purpose: visualize pct cover
# notes: 3 subreps make up a single eu

# results: model is not fitting well, but...
# the early mix impact on cover depended on the weather
# other cc trts' impacts on cover did not depend on the weather 
# nothing depended on tillage or straw removal
# decreases in covercrop cover were compensated for by increases in 'other'
# soil coverage was the same, regardless of cctrt or year, was always ~30%


library(tidyverse)
library(CENTSdata)
library(patchwork)
library(scales)
library(ggh4x)

rm(list = ls())

source("code/00_color-palettes.R")

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_fallpctcover)
w <- read_csv("data/tidy_weaclass.csv")

ord.cover_cat2= c("Soil", "Cover Crop", "Weed", "Volunteer")


#--data separated by species
d_sp <- 
  eu %>% 
  left_join(y) %>% 
  mutate(year = year(date2)) %>% 
  left_join(w) %>% 
  mutate(weayear = paste(precip, te, sep  = "-"),
         straw_sym = ifelse(straw_id == "retained", "+res", "-res"),
         cover_frac = cover_pct / 100,
         cctrt_nice = case_when(
           cctrt_id == "nocc" ~ "NoCC",
           cctrt_id == "mix_E" ~ "MixEarly",
           cctrt_id == "mix_M" ~ "MixMid",
           cctrt_id == "rad_M" ~ "RadMid",
           cctrt_id == "rad_L" ~ "RadLate",
           TRUE~"XXX"
         ),
         cctrt_id = factor(cctrt_id, levels = ord.cctrt_id),
         cctrt_nice = factor(cctrt_nice, levels = ord.cctrt_niceL),
         till_id = factor(till_id, levels = ord.till_id),
         cover_cat2 = case_when(
           cover_cat2 == "soil" ~ "Soil",
           cover_cat2 == "weed" ~ "Weed",
           cover_cat2 == "covercrop" ~ "Cover Crop",
           TRUE ~ "Volunteer"
         ),
         cover_cat2 = factor(cover_cat2, levels = ord.cover_cat2),
         precip = str_to_title(precip),
         till_nice = case_when(
           till_id == "notill" ~ "No-till",
           till_id == "inversion" ~ "Inv",
           till_id == "noninversion" ~ "Non-inv",
           TRUE ~ "XXX"
         ),
         till_nice = factor(till_nice, levels = ord.till_nice)) %>% 
  unite(precip, straw_sym, col = "wea_straw", sep = ", ")
  



  
d_sp %>%
    group_by(cctrt_id, wea_straw, till_nice, cover_cat2,
             cctrt_nice) %>% 
    summarise(cover_pct = sum(cover_pct)) %>% 
  mutate(cover_cat2 = fct_rev(cover_cat2),
         #till_nice = fct_rev(till_nice)
         ) %>% 
  ggplot(aes(wea_straw, cover_pct/1200)) +
  geom_col(aes(fill = cover_cat2 
               #alpha = cover_cat2
               ), color = "black",
           size = 1.05) +
  facet_grid(till_nice ~cctrt_nice, scales = "free") +
  coord_flip() +
  # scale_alpha_manual(values = c("Soil" = 0.8,
  #                                   "Cover Crop" = 0.8,
  #                                   "Volunteer" = 0.8,
  #                                   "Weed" = 1),
  #                        guide = F) +
  scale_fill_manual(values = c("Soil" = bv1,
                               "Cover Crop" = bv2,
                               "Volunteer" = bv3,
                               "Weed" = "red"),
                    guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        legend.position = "top",
        strip.background.x = element_rect(fill = "white", 
                                          color = "white"),
        strip.text.x = element_text(size = rel(1.3)),
        panel.border = element_blank())+
  labs(y = "Fall Ground Cover (%)",
       x = NULL,
       fill = NULL) +
    scale_y_continuous(labels = label_percent(),
                       breaks = c(.25, .75)) 

ggsave("figs/fig_fall-cover.png",
       width = 8, 
       height = 4)  



# drafts ------------------------------------------------------------------


d_sp %>%
  group_by(cctrt_id, wea_straw, till_nice, cover_cat2,
           cctrt_nice) %>% 
  summarise(cover_pct = sum(cover_pct)) %>% 
  mutate(cover_cat2 = fct_rev(cover_cat2),
         #till_nice = fct_rev(till_nice)
  ) %>% 
  ggplot(aes(till_nice, cover_pct/1200)) +
  geom_col(aes(fill = cover_cat2), color = "black", size = 1.1) +
  facet_grid(wea_straw ~cctrt_nice, scales = "free") +
  coord_flip() +
  scale_fill_manual(values = c("Soil" = bv1,
                               "Cover Crop" = bv2,
                               "Volunteer" = bv3,
                               "Weed" = "red"),
                    guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        legend.position = "top",
        strip.background.x = element_rect(fill = "white", 
                                          color = "white"),
        strip.text.x = element_text(size = rel(1.3)),
        panel.border = element_blank())+
  labs(y = "Fall Ground Coverage (%)",
       x = NULL,
       fill = "Cover Category") +
  scale_y_continuous(labels = label_percent(),
                     breaks = c(.25, .75)) 

ggsave("figs/fig_fall-cover.png",
       width = 8, 
       height = 4)

# separate reps -----------------------------------------------------------



d_sp %>%
  group_by(cctrt_id, wea_straw, till_nice, cover_cat2,
           cctrt_nice, block_id) %>% 
  summarise(cover_pct = sum(cover_pct)) %>% 
  mutate(cover_cat2 = fct_rev(cover_cat2),
         #till_nice = fct_rev(till_nice)
  ) %>% 
  ggplot(aes(block_id, cover_pct/300)) +
  geom_col(aes(fill = cover_cat2), color = "black") +
  facet_nested(wea_straw + till_nice ~cctrt_nice, scales = "free", 
               solo_line = TRUE) +
  coord_flip() +
  scale_fill_manual(values = c("Soil" = bv1,
                               "Cover Crop" = bv2,
                               "Volunteer" = bv3,
                               "Weed" = "red"),
                    guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "top",
        strip.background.x = element_rect(fill = "white", 
                                          color = "white"),
        strip.text.x = element_text(size = rel(1.3)),
        panel.border = element_blank(),
        panel.spacing = unit(0,"lines"),
        strip.background.y=element_rect(color="grey30", fill="grey90"),
        #panel.border=element_rect(color="grey90")
        )+
  labs(y = "Fall ground coverage (%)",
       x = NULL,
       fill = "Category") +
  scale_y_continuous(labels = label_percent(),
                     breaks = c(.25, .75)) 

ggsave("figs/fig_fall-cover-reps.png",
       width = 7, 
       height = 7)
