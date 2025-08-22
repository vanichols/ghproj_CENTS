#--show rank changes of farmer typologies
#--created 22 aug 2024

rm(list = ls())

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


d1 %>% 
  ggplot(aes(farmer_type, rank)) +
  geom_line(aes(group = cropsys, color = cc, linetype = straw), 
            linewidth = 2) +
  scale_y_reverse() +
  scale_linetype_manual(values = c("solid", "solid")) +
  facet_wrap(~till)

