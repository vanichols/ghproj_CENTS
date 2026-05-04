#--make a bump chart of rankings
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
    name == "fallvegvalue" ~ "Ecol",
    name == "fallbio" ~ "Biom",
    name == "yield" ~ "Grai",
    name == "minitox" ~ "Safe",
    name == "perennweeds" ~ "Weed",
    name == "pctcov" ~ "Soil",
  ))


# 3. make nice lables cc ------------------------------------------------

d3 <- 
  MakeNiceLabels(d2) %>%  
  mutate(straw_nice = ifelse(straw_id == "retained", "(+)Residue", "(-)Residue"),
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


d_smy <- 
  d5 |> 
  select(cctrt_nice, till_nice, straw_nice, value_smy) |> 
  distinct() |> 
  mutate(value_smy = round(value_smy, 2))


d6 <- 
  d_smy |> 
  mutate(straw_nice2 = str_remove(straw_nice, "Residue"),
         sys_nice = paste0(till_nice, straw_nice2)) |> 
  select(cctrt_nice, sys_nice, value_smy) |> 
  group_by(sys_nice) |>
  arrange(-value_smy) |> 
    mutate(rank = 1:n()) |> 
  arrange(sys_nice)

d6 %>% 
  ggplot(aes(sys_nice, rank, group = cctrt_nice)) +
  geom_line(size = 2, aes(color = cctrt_nice)) +
  geom_point(size = 6, aes(shape = cctrt_nice, fill = cctrt_nice, color = cctrt_nice)) +
  scale_y_reverse() +
  guides(
    color = guide_legend(reverse = TRUE, override.aes = list(size = 1)),
    fill = guide_legend(reverse = TRUE),
    shape = guide_legend(reverse = TRUE, override.aes = list(size = 4))) +
  guides(
    linetype = guide_legend(reverse = TRUE, override.aes = list(size = 1))) +
  labs(x = NULL,
       y = "Rank",
       color = "Cover crop system",
       shape = "Cover crop system",
       fill = "Cover crop system") +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_color_manual(values = c( 
    "NoCC" = hue_nocc,
    "MixE" = hue_mixe,
    "MixM" = hue_mixm,
    "RadM" = hue_radm,
    "RadL" = hue_radl)) +
  scale_fill_manual(values = c( 
    "NoCC" = hue_nocc,
    "MixE" = hue_mixe,
    "MixM" = hue_mixm,
    "RadM" = hue_radm,
    "RadL" = hue_radl)) +
  theme_bw() +
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_blank(),
        legend.position = "right",
        legend.title.position = "top") 

ggsave("figs/fig6_ranking-bump-chart.png", height = 4, width = 7.5)
