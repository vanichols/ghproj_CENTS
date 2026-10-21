#--make a bump chart of rankings
rm(list = ls())

library(tidyverse)
library(CENTSdata)
library(ggh4x)
library(patchwork)

source("code/00_color-palettes.R")

metric_cols <- c("#8DA0CB", "#66C2A5", "#E5B93C", "#D95F5F", "#9B7EBD", "#7FB069")

# 1. data -----------------------------------------------------------------

d1 <- 
  read_csv("data/tidy_system-winning-weight-profile.csv")

# 2. make nice lables cc ------------------------------------------------

d2 <- 
  MakeNiceLabels(d1) %>%  
  mutate(straw_nice = ifelse(straw_id == "retained", "(+)Straw", "(-)Straw"),
         till_nice = case_when(
           till_nice == "Inv"~"Inversion",
           till_nice == "Surf"~"Surface",
           TRUE ~ "No-till"
         ),
         till_nice = factor(till_nice, 
                            levels = c("Inversion", "Surface", "No-till")))


# Order cropping-system rows by overall median rank
cc_order <- 
  d2 |>
  group_by(cctrt_nice) |>
  summarise(m = median(rank_med)) |>
  arrange(desc(m)) |>
  pull(cctrt_nice)

win_profile <- 
  d2 |> 
  mutate(cctrt_nice = factor(cctrt_nice, levels = cc_order))

metrics <- d2 |> pull(metric) |> unique()

pB <- ggplot(win_profile,
             aes(x = mean_weight, y = interaction(cctrt_nice, straw_nice, sep = " "), fill = metric)) +
  geom_col(width = 0.75, colour = "white", linewidth = 0.3) +
  facet_wrap(~ till_nice, ncol = 1, scales = "free_y") +
  scale_x_continuous(labels = percent, expand = c(0, 0)) +
  scale_fill_manual(values = setNames(metric_cols, metrics), name = "Metric") +
  labs(title = "Weight profile under which each winning system ranks #1",
       subtitle = "Only systems that win under at least one weighting are shown",
       x = "Average weight on each metric", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        plot.title.position = "plot",
        strip.text = element_text(face = "bold"))

pB
