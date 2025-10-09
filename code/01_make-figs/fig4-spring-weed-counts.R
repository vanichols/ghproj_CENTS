#--spring weed counts
#--creatd 9 oct 2025

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

draw <- 
  cents_spweedcount %>% 
  left_join(cents_eukey) %>% 
  mutate(weed_type2 = ifelse(weed_type %in% c("dicot", "monocot"), "A", "P"))

d <- 
  draw %>% 
  MakeNiceLabels(.)

d1 <- 
  d %>% 
  group_by(cctrt_nice, till_nice, weed_type2) %>% 
  summarise(count = mean(count)) %>% 
  mutate(weed_type_nice = ifelse(weed_type2 == "A", "Annual", "Perennial"))
  

#--this needs to be fixed, somehow indicate tot and P separately
d2 <- 
  d1 %>% 
  mutate(star = ifelse( (cctrt_nice == "MixE" & till_nice != "Inv"), 
                        "*", " "))
  
# 2. fig ------------------------------------------------------------------


d1 %>% 
  ggplot(aes(cctrt_nice, count)) +
  geom_col(aes(fill = weed_type_nice)) +
  geom_text(data = d2, aes(x = cctrt_nice, y = count + 10, label = star)) +
  facet_grid(~till_nice) +
  scale_fill_manual(values = c(bv1, bv2)) +
  labs(x = "Cover crop system", 
       y = "Total weed count", 
       fill = "Weed growth habit") +
  th1 +
  labs(title = "still working on stars")

ggsave("figs/fig4_spring-weeds.png", 
       width = 6.8, height = 4)
