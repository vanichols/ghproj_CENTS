#--make a circular barplot
#--average values over years...bc some things are 2020 and some are 2018 and 2019
#--this is sloppy with regards to averaging and scaling right now, but...
#--7 april 2025 update with new tillage nomenclature
#--12 june 2025 - need to change to match pesentation order (top half service)

rm(list = ls())

library(tidyverse)
library(CENTSdata)
library(ggh4x)
library(patchwork)

source("code/00_color-palettes.R")

# 1. data -----------------------------------------------------------------

d1 <- 
  read_csv("data/tidy_tradeoffs.csv")


# 2. labels for metrics -------------------------------------------------------------

d2 <- 
  d1 %>% 
  mutate(name_nice = case_when(
    name == "dm_gm2" ~ "Bio",
    name == "yield_dry_Mgha" ~ "Gra",
    name == "benef" ~ "Eco",
    name == "load_ha" ~ "Tox",
    name == "harm" ~ "Har",
    name == "count" ~ "Leg",
  ))

# 3. make nice lables cc ------------------------------------------------
  
d3 <- 
  d2 %>% 
  mutate(cctrt_nice = case_when(
    cctrt_id == "nocc" ~ "NoCC",
    cctrt_id == "mix_E" ~ "MixEarly",
    cctrt_id == "mix_M" ~ "MixMid",
    cctrt_id == "rad_M" ~ "RadMid",
    cctrt_id == "rad_L" ~ "RadLate",
    TRUE~"XXX"
  ),
  cctrt_id = factor(cctrt_id, levels = ord.cctrt_id),
  cctrt_nice = factor(cctrt_nice, levels = ord.cctrt_niceL))


# 4. make nice tillage ----------------------------------------------------

d4 <- 
  d3 %>% 
  mutate(
    till_id = factor(till_id, levels = ord.till_id),
    till_nice = case_when(
      till_id == "notill" ~ "No-till",
      till_id == "inversion" ~ "Inv",
      till_id == "surface" ~ "Surf",
      TRUE ~ "XXX"
    ),
    till_nice = factor(till_nice, levels = ord.till_nice))


# 5. make nice straw ----------------------------------------------------

d5 <- 
  d4 %>% 
  mutate(straw_nice = ifelse(straw_id == "retained", "+res", "-res"))



# 6. net value ------------------------------------------------------------

#--need to double check this....

d6 <- 
  d5 %>%  
  mutate(value3 = ifelse(cat == "service", value2, -value2)) %>% 
  group_by(cctrt_id, till_id, straw_id) %>% 
  summarise(netval = sum(value3)) %>% 
  mutate(netval = round(netval, 1)) %>% 
  arrange(-netval)

#--mix_E had largest value in notill, but lowest value in inversion, interesting
d6 %>% 
  ggplot(aes(cctrt_id, till_id)) +
  geom_point(aes(size = netval, color = netval)) +
  facet_grid(.~straw_id) + 
  scale_size_continuous(range = c(2, 15)) +
  scale_color_viridis_c()


# 7. fig data -----------------------------------------------------------------

d7 <- 
  d5 %>% 
  left_join(d6) %>%
  ungroup() %>%
  mutate(cat = str_to_sentence(cat),
         order = 1:n()) 

#--these both score high net wise
#mix_E noninv removed
#rad_M noninv removed

d6 %>% 
  filter(till_id == "notill")
#--notill, retained are both good

d6 %>% 
  filter(till_id == "inversion")

d7 %>% 
  filter(value2 == 1)

d7 %>% 
  filter(value2 == max(value2))

#--not sure what to highlight yet
#--maybe 'best' of each cc?
d_hi1 <- 
  d7 %>% 
  filter(netval == max(netval))

d_hi2 <- 
  d7 %>% 
  filter(netval == min(netval))

d_hi3 <- 
  d7 %>% 
  filter(netval == 0.8,
         cctrt_id == "rad_M",
         till_id == "notill")

d_hi4 <- 
  d7 %>% 
  filter(netval == 0.2,
         cctrt_id == "rad_M",
         till_id == "notill")
         
         


# 8. figs -----------------------------------------------------------------

p1 <- 
  d7 %>% 
  ggplot(aes(reorder(str_wrap(name_nice, 5), order), value2)) +
  geom_rect(data = d_hi1, 
            fill = "gold", xmin = -Inf,xmax = Inf, alpha = 0.1,
            ymin = -Inf,ymax = Inf) +
  geom_rect(data = d_hi2, 
            fill = "gold", xmin = -Inf,xmax = Inf, alpha = 0.1,
            ymin = -Inf,ymax = Inf) +
  geom_rect(data = d_hi3, 
            fill = "gold", xmin = -Inf,xmax = Inf, alpha = 0.1,
            ymin = -Inf,ymax = Inf) +
  geom_rect(data = d_hi4, 
            fill = "gold", xmin = -Inf,xmax = Inf, alpha = 0.1,
            ymin = -Inf,ymax = Inf) +
  geom_col(aes(fill = cat), color = "black", show.legend = T) +
  geom_label(aes(x = 0, y = 0, label = netval)) +
  facet_nested(till_nice + straw_nice ~cctrt_nice) +
  coord_polar(clip = "off") +
  labs(x = NULL,
       y = NULL,
       fill = NULL) +
  theme_bw() +
  theme(axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_blank(),
        legend.position = "top",
        strip.background.x = element_rect(fill = "white", 
                                          color = "white"),
        strip.text = element_text(size = rel(1.3))) +
  scale_fill_manual(values = c("red", "dodgerblue4")) 

p1
ggsave("figs/fig_star-chart-all.png", height = 10, width = 10)


##--not using any more, but leaving for reference

# highlighted ind plots ---------------------------------------------------

#--highlighted examples
p_hi1 <- 
  d_hi1 %>% 
  ggplot(aes(reorder(str_wrap(name_nice, 5), order), value2)) +
  geom_col(aes(fill = cat), color = "black") +
  geom_label(aes(x = 0, y = 0, label = netval)) +
  facet_nested(till_nice + straw_nice ~cctrt_nice) +
  coord_polar(clip = "off") +
  labs(x = NULL,
       y = NULL,
       fill = NULL) +
  theme_bw() +
  theme(axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_blank(),
        legend.position = "top",
        strip.background.x = element_rect(fill = "white", 
                                          color = "white"),
        strip.text = element_text(size = rel(1.3))) +
  scale_fill_manual(values = c("red", "dodgerblue4")) 

p_hi1

#--highlighted examples
p_hi2 <- 
  d_hi2 %>% 
  ggplot(aes(reorder(str_wrap(name_nice, 5), order), value2)) +
  geom_col(aes(fill = cat), color = "black") +
  geom_label(aes(x = 0, y = 0, label = netval)) +
  facet_nested(till_nice + straw_nice ~cctrt_nice) +
  coord_polar(clip = "off") +
  labs(x = NULL,
       y = NULL,
       fill = NULL) +
  theme_bw() +
  theme(axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_blank(),
        legend.position = "top",
        strip.background.x = element_rect(fill = "white", 
                                          color = "white"),
        strip.text = element_text(size = rel(1.3))) +
  scale_fill_manual(values = c("red", "dodgerblue4")) 

p_hi2

#--highlighted examples
p_hi3 <- 
  d_hi3 %>% 
  ggplot(aes(reorder(str_wrap(name_nice, 5), order), value2)) +
  geom_col(aes(fill = cat), color = "black") +
  geom_label(aes(x = 0, y = 0, label = netval)) +
  facet_nested(till_nice + straw_nice ~cctrt_nice) +
  coord_polar(clip = "off") +
  labs(x = NULL,
       y = NULL,
       fill = NULL) +
  theme_bw() +
  theme(axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_blank(),
        legend.position = "top",
        strip.background.x = element_rect(fill = "white", 
                                          color = "white"),
        strip.text = element_text(size = rel(1.3))) +
  scale_fill_manual(values = c("red", "dodgerblue4")) 

p_hi3

p_hi4 <- 
  d_hi4 %>% 
  ggplot(aes(reorder(str_wrap(name_nice, 5), order), value2)) +
  geom_col(aes(fill = cat), color = "black") +
  geom_label(aes(x = 0, y = 0, label = netval)) +
  facet_nested(till_nice + straw_nice ~cctrt_nice) +
  coord_polar(clip = "off") +
  labs(x = NULL,
       y = NULL,
       fill = NULL) +
  theme_bw() +
  theme(axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_blank(),
        legend.position = "top",
        strip.background.x = element_rect(fill = "white", 
                                          color = "white"),
        strip.text = element_text(size = rel(1.3))) +
  scale_fill_manual(values = c("red", "dodgerblue4")) 

# combine plots -----------------------------------------------------------

p_square <- 
  (p_hi2 + p_hi4) / (p_hi1 + p_hi3) +
  plot_layout(guides = "collect")& theme(legend.position = 'top') 

p_square

ggsave("figs/fig_star-chart-highlights.png", height = 6, width = 5)

p_all <- 
  p1 + p_square 

ggsave("figs/fig_star-chart-all.png", p_all, 
       height = 12, width = 10)



design <- "
66666
11135
11124
"    

p1 + p_hi1 + p_hi2 + p_hi3 + p_hi4 + guide_area() + 
  plot_layout(design=design, guides = "collect", heights = c(1, 3)) 

ggsave("figs/fig_star-chart-all.png", height = 12, width = 10)


p1 +  (p_hi1 / p_hi2)  + (p_hi3 / p_hi4)  +
  plot_layout(guides = "collect", 
              widths = c(3, 1, 1),
              heights = c(3,3,1)
              ) 

#ggsave("figs/fig_star-chart-all.png", height = 12, width = 10)

