#--make a circular barplot
#--average values over years...bc some things are 2020 and some are 2018 and 2019
#--this is sloppy with regards to averaging and scaling right now, but...

library(tidyverse)
library(CENTSdata)
library(ggh4x)
library(patchwork)

source("code/00_color-palettes.R")

spv <- 
  read_csv("data/tidy_spvalue.csv") %>% 
  select(eppo_code, harm, benef)

fcov <- 
  cents_fallpctcover %>% 
  mutate(year = year(date2)) %>% 
  group_by(eu_id, year, eppo_code) %>% 
  summarise(cover_pct = sum(cover_pct)) %>% 
  left_join(spv)


#--the good (biomass, crop yields, eco value)
bio <- 
  cents_fallbio %>% 
  mutate(year = year(date2)) %>% 
  group_by(eu_id) %>% 
  summarise(dm_gm2 = sum(dm_gm2, na.rm = T))

yld <- 
  cents_cropyields %>% 
  mutate(year = year(date2)) %>% 
  group_by(eu_id) %>% 
  summarise(yield_dry_Mgha = mean(yield_dry_Mgha))
  
eco <- 
  fcov %>% 
  group_by(eu_id) %>% 
  summarise(benef = sum(benef*cover_pct))

ser<- 
  bio %>% 
  left_join(yld) %>% 
  left_join(eco) %>% 
  pivot_longer(dm_gm2:benef) %>% 
  mutate(cat = "services")

#--the bad (pli, # circ in spring, agronomic harm)
pli <- 
  read_csv("data/tidy_pesticide-load-by-eu.csv") %>% 
  group_by(eu_id) %>% 
  summarise(load_ha = sum(load_ha))

har <- 
  fcov %>% 
  group_by(eu_id) %>% 
  summarise(harm = sum(harm*cover_pct))

leg <- 
  cents_spweedcount %>% 
  filter(weed_type == "cirar") %>% 
  group_by(eu_id) %>% 
  summarise(count = sum(count))

dis <- 
  pli %>% 
  left_join(har) %>% 
  left_join(leg) %>% 
  pivot_longer(load_ha:count) %>% 
  mutate(cat = "disservices")


# combine and scale -------------------------------------------------------
##--needs work
# https://r-graph-gallery.com/web-circular-barplot-with-R-and-ggplot2.html

dat <- 
  ser %>% 
  bind_rows(dis) %>% 
  mutate(name = fct_inorder(name)) %>% 
  group_by(name) %>% 
  #not sure how to scale from 0-1
  mutate(value2 = value/max(value))

#--single example
p1 <- 
  dat %>% 
  left_join(cents_eukey) %>%
  group_by(cctrt_id, till_id, straw_id, name, cat) %>% 
  summarise(value2 = mean(value2))  %>%
  filter(till_id == "notill" & cctrt_id %in% c("mix_E", "rad_M")) %>% 
  ggplot(aes(name, value2)) +
  geom_col(aes(fill = cat), color = "black") +
  facet_grid(till_id + straw_id~cctrt_id) +
  coord_polar() +
    theme_bw() +
  theme(axis.text.y = element_blank()) +
    scale_fill_manual(values = c(av1, "dodgerblue")) 
  

p2 <- 
  dat %>% 
  left_join(cents_eukey) %>%
  group_by(cctrt_id, till_id, straw_id, name, cat) %>% 
  summarise(value2 = mean(value2))  %>% 
  ggplot(aes(name, value2)) +
  geom_col(aes(fill = cat), color = "black") +
  facet_nested(till_id + straw_id ~cctrt_id) +
  coord_polar() +
  theme_bw() +
  theme(axis.text = element_blank()) +
  scale_fill_manual(values = c(av1, "dodgerblue")) 

design <- "
3322
1122
"    
#p1 + p2 + guide_area() + plot_layout(design=design, guides = "collect") 

p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "top")
