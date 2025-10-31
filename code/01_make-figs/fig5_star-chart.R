#--make a circular barplot
#//look at 6 perfomance categories together

rm(list = ls())

library(tidyverse)
library(CENTSdata)
library(ggh4x)
library(patchwork)

source("code/00_color-palettes.R")

# 1. data -----------------------------------------------------------------

d1 <- 
  read_csv("data/tidy_multicriteria-values.csv")


# 2. labels for metrics -------------------------------------------------------------

d1 %>% 
  pull(name) %>% 
  unique()

d2 <- 
  d1 %>% 
  mutate(name_nice = case_when(
    name == "fallvegvalue" ~ "Eco",
    name == "fallbio" ~ "Bio",
    name == "yield" ~ "Gra",
    name == "minitox" ~ "Saf",
    name == "perennweeds" ~ "Per",
    name == "pctcov" ~ "Soi",
  ))


# 3. make nice lables cc ------------------------------------------------

d3 <- 
  MakeNiceLabels(d2) %>%  
  mutate(straw_nice = ifelse(straw_id == "retained", "+res", "-res"),
         till_nice = case_when(
           till_nice == "Inv"~"Inversion",
           till_nice == "Surf"~"Surface",
           TRUE ~ "No-till"
         ),
         till_nice = factor(till_nice, 
                            levels = c("Inversion", "Surface", "No-till")))


# 4. total score ----------------------------------------------------------

d4 <- 
  d3 %>% 
  group_by(pick(contains("nice"), -name_nice)) %>% 
  summarise(value_smy = mean(value))
  

# 5. fig ------------------------------------------------------------------

d5 <- 
  d3 %>% 
  left_join(d4)


d5 %>% 
  ggplot(aes((str_wrap(name_nice, 5)), value)) +
  geom_col(aes(fill = value_smy),
           color = "black") +
  scale_fill_viridis_c() +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  facet_nested(cctrt_nice~till_nice + straw_nice,
               switch = "y") +
  coord_polar(clip = "off") +
  labs(x = NULL,
       y = NULL,
       fill = "Total relative value") +
  theme_bw() +
  theme(axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.title.position = "top",
        strip.background.x = element_rect(fill = "white", 
                                          color = "white"),
        strip.background.y = element_rect(fill = "white", 
                                          color = "white"),
        strip.text = element_text(size = rel(1.3))) 


ggsave("figs/fig5_total-value.png", height = 9, width = 9)


