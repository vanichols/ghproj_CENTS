#--show rank changes of farmer typologies
#--created 22 aug 2024

rm(list = ls())

theme_set(theme_bw())
library(tidyverse)
library(CENTSdata)
library(ggh4x)
library(patchwork)
library(ggalluvial)
library(plotrix)

source("code/00_color-palettes.R")

# 1. data -----------------------------------------------------------------

d1a <- read_csv("data/tidy_typology-values.csv")

d1 <- 
  d1a %>% 
  separate(cropsys, into = c("till", "cc", "straw"), sep = " ", remove = F) %>% 
  group_by(farmer_type, till) %>% 
  arrange(-valuetot) %>% 
  mutate(rank = 1:n()) %>% 
  arrange(farmer_type, rank)

#--

d1 %>% 
  ggplot(aes(farmer_type, rank)) +
  geom_line(aes(group = cropsys, color = cc, alpha = straw), 
            linewidth = 2) +
  scale_y_reverse() +
  scale_alpha_manual(values = c(1, 0.5)) +
  facet_wrap(~till) +
  theme(axis.ticks.y = element_blank(),
        legend.position = "top",
        strip.background.x = element_rect(fill = "white", 
                                          color = "white"),
        strip.text.x = element_text(size = rel(1.3)),
        panel.border = element_blank()
  )


