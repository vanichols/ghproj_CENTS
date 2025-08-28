# created 13 june 2025 
# visualize weed stuff

library(tidyverse)
library(readxl)
library(CENTSdata)
library(ggrepel)

d1 <- 
  read_csv("data/tidy_spvalue.csv") %>% 
  mutate(value2 = sc_value2_max,
         value2 = ifelse(cat == "harm", -value2, value2))

# 1. data -----------------------------------------------------------------

#--look at the weeds
#--this is a good figure
d1 %>% 
  ggplot(aes(eppo_code, value2)) +
  geom_point(aes(color = cat), size = 3) +
  scale_color_manual(values = c("purple", "red", "green4")) +
  coord_flip()


d1 %>%
  filter(cat == "harm") %>% 
  mutate(harmval = -value2) %>% 
  select(eppo_code, latin_name, harmval) %>% 
  left_join(d1 %>% 
              filter(cat != "harm") %>% 
              select(eppo_code, latin_name, cat, value2)) %>% 
  filter(eppo_code != "soil") %>% 
  mutate(eppo_code = str_to_upper(eppo_code)) %>% 
  ggplot(aes(harmval, value2)) +
  geom_point(aes(color = cat), size = 5) +
  geom_label_repel(aes(label = eppo_code)) +
  labs(x = "Agronomic harm")
  facet_grid(.~cat)
