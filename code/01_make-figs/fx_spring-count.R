#--make crop yield fig
#--should add regression of thistle count on faba yield underneath? or thistle by cctrt

library(SEXYrye)
library(tidyverse)
library(CENTSdata)

source("code/00_color-palettes.R")

cents_spweedcount %>% 
  left_join(cents_eukey) %>%
  mutate(year = year(date2)) %>% 
  filter(weed_type %in% c("cirar")) %>% 
  ggplot(aes(cctrt_id, count)) +
  geom_col() +
  facet_grid(.~year)
