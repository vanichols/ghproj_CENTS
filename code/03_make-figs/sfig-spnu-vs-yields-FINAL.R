#--spring weed counts
#--creatd 9 oct 2025
#--something is goofy - the model estimates don't match the figure 
#--model says inv has a mean of like 500 pl m2...

library(tidyverse)
library(CENTSdata)
library(patchwork)
library(ggh4x)
library(ggrepel)
library(ggpubr)
library(ggpattern)
library(ggbeeswarm)

rm(list = ls())


# fig stuff ---------------------------------------------------------------

source("code/00_color-palettes.R")

theme_set(theme_bw())

th1 <-   theme(axis.ticks.y = element_blank(),
               axis.text.x = element_text(angle = 45, hjust = 1),
               legend.position = "top",
               strip.background.x = element_rect(fill = "white", 
                                                 color = "black"),
               strip.text.x = element_text(size = rel(1.3)),
               panel.border = element_blank())



# 1. data -----------------------------------------------------------------

spwe <- read_csv("data/stats/figs_emmeans/emmeans-sfig-spweedresponses.csv")

d <- 
  cents_eukey |> 
  select(eu_id, till_id, cctrt_id, straw_id) |> 
  left_join(spwe, relationship = 'many-to-many')

d1 <- 
  d |> 
  left_join(cents_cropyields %>% filter(crop == "faba bean")) %>% 
  mutate(weed_type_nice = ifelse(weed_type2 == "A", "Annual weeds", "Perennial weeds"))


d1 %>% 
  ggplot(aes(response, yield_dry_Mgha)) +
  geom_point(aes(color = weed_type_nice, shape = weed_type_nice), 
             show.legend = F) +
  geom_smooth(method = "lm", se = F, color = hue_radl) +
  scale_color_manual(values = c(bv1, bv2, "black")) +
  labs(x = myweedcountlab,
       y = myyieldlab_faba,
       color = NULL,
       shape = NULL) +
  facet_grid(.~ weed_type_nice, scale = "free") +
  th1

corrstar <- 
  tibble(weed_type_nice = "Perennial weeds", 
         xplace = 10,
         yplace = 5)
  
d1 %>% 
    ggplot(aes(response, yield_dry_Mgha)) +
    geom_point(aes(color = weed_type_nice, shape = weed_type_nice), 
               show.legend = F,
               size = 2) +
  geom_text(data = corrstar, 
            aes(x = xplace, 
                y = yplace),
            label = "*p<0.001", 
            size = 4) +
    geom_smooth(method = "lm", se = F, color = hue_radl) +
    scale_color_manual(values = c(bv1, bv2, "black")) +
  scale_y_continuous(limits = c(0, 6)) +
    labs(x = myweedcountlab,
         y = myyieldlab_faba,
         color = NULL,
         shape = NULL) +
    facet_grid(.~ weed_type_nice, scale = "free") +
    th1 +
  theme(legend.key = element_rect(fill = "white",
                                  colour = "black"))

ggsave("data/stats/supp_tables/sfig_spring-weeds-and-yields.png",
       width = 7, height = 8)
