#--make a circular barplots example for presentation

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
    TRUE ~ "XXXX"),
  name_long = case_when(
    name == "dm_gm2" ~ "Biomass",
    name == "yield_dry_Mgha" ~ "Grain yields",
    name == "benef" ~ "Ecological value",
    name == "load_ha" ~ "Toxicity",
    name == "harm" ~ "Agronmic harm",
    name == "count" ~ "Perennial weed legacy",
    TRUE ~ "XXXX"),
  myorder = case_when(
    name == "dm_gm2" ~ 5,
    name == "yield_dry_Mgha" ~ 6,
    name == "benef" ~ 1,
    name == "load_ha" ~ 2,
    name == "harm" ~ 3,
    name == "count" ~ 4,
    TRUE ~ 9999)
  ) %>% 
  select(cat, myorder, name_nice, name_long) %>% 
  distinct() %>% 
  mutate(cat = str_to_sentence(cat))

# 3. fix it up ------------------------------------------------
  
a1 <- 
  d2 %>% 
  mutate(value2 = c(1, 0, 0, 0, 0, 0))

a2 <- 
  d2 %>% 
  mutate(value2 = c(0, 1, 0, 0, 0, 0))

a3 <- 
  d2 %>% 
  mutate(value2 = c(0, 0, 1, 0, 0, 0))

a4 <- 
  d2 %>% 
  mutate(value2 = c(0, 0, 0, 1, 0, 0))

a5 <- 
  d2 %>% 
  mutate(value2 = c(0, 0, 0, 0, 1, 0))

a6 <- 
  d2 %>% 
  mutate(value2 = c(0, 0, 0, 0, 0, 1))

d3 <- 
  a1 %>% 
  bind_rows(a2) %>%
  bind_rows(a3) %>% 
  bind_rows(a4) %>% 
  bind_rows(a5) %>% 
  bind_rows(a6)  %>% 
  mutate(name_long = fct_inorder(name_long))

# figure ------------------------------------------------------------------

d3 %>% 
  ggplot(aes(reorder(str_wrap(name_nice, 5), myorder), value2)) +
  geom_col(aes(fill = cat), show.legend = F) +
  geom_vline(xintercept = 1.5) +
  geom_vline(xintercept = 4.5) +
  facet_wrap( ~name_long, nrow = 2) +
  coord_polar(
    clip = "off",
    start = 0.52,
    direction = 1) +
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
        strip.text = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.5))) +
  scale_fill_manual(values = c("red", "dodgerblue4")) 

ggsave("figs/pres_star-chart-all.png", height = 8, width = 8)
