#--make a bump chart of rankings
rm(list = ls())

library(tidyverse)
library(CENTSdata)
library(ggh4x)
library(patchwork)

source("code/00_color-palettes.R")

# 1. data -----------------------------------------------------------------

d1 <- 
  read_csv("data/tidy_typology-totvalues.csv")


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



# 3. rankings -------------------------------------------------------------

d3 <- 
  d2 |> 
  mutate(cropsys = paste(cctrt_id, till_id, straw_id)) |>
  arrange(farmer_type, till_id, -valuetot) |> 
  group_by(farmer_type, till_id) |>
  mutate(rank = 1:n())


# 3. fig ------------------------------------------------------------------

d3 |>  
  ggplot(aes(farmer_type, valuetot, group = cropsys)) +
  geom_line(linewidth = 1.5, aes(color = cctrt_nice, linetype = straw_nice)) +
  guides(
    color = guide_legend(reverse = TRUE, override.aes = list(size = 1)),
    #fill = guide_legend(reverse = TRUE),
    linetype = guide_legend(reverse = TRUE, override.aes = list(size = 4))) +
  # guides(
  #   linetype = guide_legend(reverse = TRUE, override.aes = list(size = 1))) +
  #scale_y_reverse(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  labs(x = NULL,
       y = "Relative value (0-1)",
       color = "Cover crop system",
       linewidth = "Tillage system",
       linetype = "Residue system") +
  scale_color_manual(values = c( 
    "NoCC" = hue_nocc,
    "MixE" = hue_mixe,
    "MixM" = hue_mixm,
    "RadM" = hue_radm,
    "RadL" = hue_radl)) +
  scale_linetype_manual(values = c(
    "(-)Straw" = 11,
    "(+)Straw" = "solid"
  )) +
  facet_grid(.~till_nice) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_blank(),
        legend.position = "right",
        legend.title.position = "top", 
        strip.background.x = element_rect(fill = "white", 
                                          color = "black"),
        strip.text.x = element_text(size = rel(1.3))) 

ggsave("figs/fig7_typology-specific-value.png", height = 7.5, width = 9)

#--trying other formats...
d3 |>  
  ggplot(aes(farmer_type, valuetot, group = cropsys)) +
  geom_line(linewidth = 1.5, aes(color = cctrt_nice, linetype = straw_nice)) +
  guides(
    color = guide_legend(reverse = TRUE, override.aes = list(size = 1)),
    #fill = guide_legend(reverse = TRUE),
    linetype = guide_legend(reverse = TRUE, override.aes = list(size = 4))) +
  # guides(
  #   linetype = guide_legend(reverse = TRUE, override.aes = list(size = 1))) +
  #scale_y_reverse(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  labs(x = NULL,
       y = "Relative value (0-1)",
       color = "Cover crop system",
       linewidth = "Tillage system",
       linetype = "Residue system") +
  scale_color_manual(values = c( 
    "NoCC" = hue_nocc,
    "MixE" = hue_mixe,
    "MixM" = hue_mixm,
    "RadM" = hue_radm,
    "RadL" = hue_radl)) +
  scale_linetype_manual(values = c(
    "(-)Straw" = "dotted",
    "(+)Straw" = "solid"
  )) +
  facet_grid(till_nice~.) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_blank(),
        legend.position = "right",
        legend.title.position = "top") 
