#--make a circular barplot
#--average values over years...bc some things are 2020 and some are 2018 and 2019
#--this is sloppy with regards to averaging and scaling right now, but...

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
      till_id == "noninversion" ~ "Non-inv",
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


# 7. figs -----------------------------------------------------------------

d7 <- 
  d5 %>% 
  left_join(d6) %>% 
  mutate(cat = str_to_sentence(cat))

#--pick which to highlight
d6

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

d7 %>% 
  ungroup() %>%
  mutate(order = 1:n()) %>% 
  ggplot(aes(reorder(str_wrap(name_nice, 5), order), value2)) +
  geom_rect(data = . %>% 
              filter(cctrt_id %in% c("mix_E") & 
                       till_id %in% c("noninversion") & 
                       straw_id %in% c("removed")), 
            fill = "gold", xmin = -Inf,xmax = Inf, alpha = 0.1,
            ymin = -Inf,ymax = Inf) +
  geom_rect(data = . %>% 
              filter(cctrt_id %in% c("rad_M") & 
                       till_id %in% c("noninversion") & 
                       straw_id %in% c("removed")), 
            fill = "gold", xmin = -Inf,xmax = Inf, alpha = 0.1,
            ymin = -Inf,ymax = Inf) +
  geom_col(aes(fill = cat), color = "black") +
  geom_label(aes(x = 0, y = 0, label = netval)) +
  labs(x = NULL,
       y = NULL,
       fill = NULL) +
  facet_nested(till_nice + straw_nice ~cctrt_nice) +
  coord_polar(clip = "off") +
  theme_bw() +
  theme(axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_blank(),
        legend.position = "top",
        strip.background.x = element_rect(fill = "white", 
                                          color = "white"),
        strip.text.x = element_text(size = rel(1.3))) +
  scale_fill_manual(values = c("red", "dodgerblue4")) 

ggsave("figs/fig_star-chart-all.png", height = 12, width = 10)

#--highlighted examples
d6
d7 %>% 
  filter(name_nice == "Gra") %>% 
  arrange(till_id, cctrt_id, straw_id)

d7 %>% 
  filter(cctrt_id %in% c("mix_E") & 
           till_id %in% c("notill") & 
           straw_id %in% c("removed")) %>% 
  ungroup() %>% 
  mutate(order = 1:n()) %>% 
  ggplot(aes(reorder(str_wrap(name_nice, 5), order), value2)) +
  geom_col(aes(fill = cat), color = "black") +
  geom_label(data = d6  %>% 
               filter(cctrt_id %in% c("mix_E") & 
                                  till_id %in% c("notill") & 
                                  straw_id %in% c("removed")),
             aes(x = 0, y = 0, label = netval)) +
  facet_nested(till_id + straw_id ~cctrt_id) +
  coord_polar(clip = "off") +
  theme_bw() +
  theme(axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank()) +
  scale_fill_manual(values = c(av1, "dodgerblue")) 


p3 <- 
  dat_p %>% 
  filter(cctrt_id %in% c("rad_M") & till_id %in% c("notill") & straw_id %in% c("retained")) %>% 
  ungroup() %>% 
  mutate(order = 1:n()) %>% 
  ggplot(aes(reorder(str_wrap(name_nice, 5), order), value2)) +
  geom_rect(data = . %>% filter(cctrt_id %in% c("mix_E") & till_id %in% c("inversion") & straw_id %in% c("removed")), 
            fill = "gold", xmin = -Inf,xmax = Inf, alpha = 0.1,
            ymin = -Inf,ymax = Inf) +
  geom_rect(data = . %>% filter(cctrt_id %in% c("rad_M") & till_id %in% c("notill") & straw_id %in% c("retained")), 
            fill = "gold", xmin = -Inf,xmax = Inf, alpha = 0.1,
            ymin = -Inf,ymax = Inf) +
  geom_col(aes(fill = cat), color = "black") +
  facet_nested(till_id + straw_id ~cctrt_id) +
  coord_polar(clip = "off") +
  theme_bw() +
  theme(axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank()) +
  scale_fill_manual(values = c(av1, "dodgerblue")) 

  
p2 + p3

#--single examples
p1 <- 
  dat_p  %>%
  filter(till_id == "notill" & cctrt_id %in% c("mix_E", "rad_M")) %>% 
  ggplot(aes(name, value2)) +
  geom_col(aes(fill = cat), color = "black") +
  facet_grid(till_id + straw_id~cctrt_id) +
  coord_polar(clip = "off") +
    theme_bw() +
  theme(axis.text.y = element_blank()) +
    scale_fill_manual(values = c(av1, "dodgerblue")) 
  

p2 <- 
  dat_p %>% 
  ggplot(aes(name, value2)) +
  geom_col(aes(fill = cat), color = "black") +
  facet_nested(till_id + straw_id ~cctrt_id) +
  coord_polar(clip = "off") +
  theme_bw() +
  theme(axis.text = element_blank()) +
  scale_fill_manual(values = c(av1, "dodgerblue")) 

design <- "
3322
1122
"    
#p1 + p2 + guide_area() + plot_layout(design=design, guides = "collect") 

p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "top")

