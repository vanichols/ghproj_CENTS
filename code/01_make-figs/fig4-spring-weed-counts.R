#--spring weed counts
#--creatd 9 oct 2025
#--need to fix sig stars and use patchwork

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
  mutate(weed_type_nice = ifelse(weed_type2 == "A", "Annual weeds", "Perennial weeds"))
  

#--this needs to be fixed, somehow indicate tot and P separately
d2a <- 
  d1 %>% 
  mutate(star1 = ifelse( (cctrt_nice == "MixE" & till_nice != "Inv"), 
                        "*", " ")) %>% 
  group_by(till_nice) %>% 
  mutate(starpos1 = max(count)+10)

d2b <- 
  d1 %>% 
  filter(weed_type2 == "P") %>% 
  mutate(star2 = ifelse( (cctrt_nice == "MixE" & till_nice != "Inv"), 
                         "*", " ")) %>% 
  group_by(till_nice, weed_type2) %>% 
  mutate(starpos2 = max(count)+5)
  
# 2. fig ------------------------------------------------------------------

plot1 <- 
  d1 %>% 
  ggplot(aes(cctrt_nice, count)) +
  geom_col(aes(fill = weed_type_nice), color = "black") +
  geom_text(data = d2a, 
            aes(x = cctrt_nice, y = starpos1, label = star1),
            size = 8) +
  geom_text(data = d2b, 
            aes(x = cctrt_nice, y = starpos2, label = star2),
            color = bv2, size = 8) +
  facet_grid(~till_nice) +
  scale_fill_manual(values = c(bv1, bv2)) +
  labs(x = "Cover crop system", 
       y = myweedcountlab, 
       fill = NULL) +
  th1 +
  theme(legend.key = element_rect(fill = "white",
                                  colour = "black"))


plot1

# 3. faba yields vs p weeds----------------------------------------------

d3a <- 
  d %>% 
  group_by(eu_id, till_id, cctrt_id) %>% 
  summarise(count = sum(count))  %>% 
  mutate(weed_type2 = "Total") 

d3 <- 
  d %>% 
  group_by(eu_id, till_id, cctrt_id, weed_type2) %>% 
  summarise(count = sum(count)) %>% 
  bind_rows(d3a) %>% 
  left_join(cents_cropyields %>% filter(crop == "faba bean")) %>% 
  mutate(weed_type_nice = ifelse(weed_type2 == "A", "Annual weeds", 
                                 ifelse(weed_type2 == "P", "Perennial weeds",
                                        "Total weeds")))


#plot2 <- 
  d3 %>% 
  ggplot(aes(count, yield_dry_Mgha)) +
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
         xplace = 75,
         yplace = 5)
  
plot2 <- 
  d3 %>% 
  filter(weed_type_nice != "Total weeds",
         #!(count == 0)
         ) %>% 
    ggplot(aes(count, yield_dry_Mgha)) +
    geom_point(aes(color = weed_type_nice, shape = weed_type_nice), 
               show.legend = F,
               size = 2) +
  geom_text(data = corrstar, 
            aes(x = xplace, 
                y = yplace),
            label = "* add ro value and p-value to both panels", 
            size = 6) +
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

plot2
  
# 4. combine --------------------------------------------------------------

plot1 / plot2

ggsave("figs/fig4_spring-weeds-and-yields.png",
       width = 7, height = 8)
