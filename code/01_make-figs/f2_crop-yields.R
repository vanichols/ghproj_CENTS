#--make crop yield fig
#--should add regression of thistle count on faba yield underneath? or thistle by cctrt

library(SEXYrye)
library(tidyverse)
library(CENTSdata)

source("code/00_color-palettes.R")

# data --------------------------------------------------------------------

em_cc <- 
  read_csv("data/em_crop-yields-by-cc.csv") %>% 
  mutate(cctrt_nice = case_when(
    cctrt_id == "nocc" ~ "NoCC",
    cctrt_id == "mix_E" ~ "MixE",
    cctrt_id == "mix_M" ~ "MixM",
    cctrt_id == "rad_M" ~ "RadM",
    cctrt_id == "rad_L" ~ "RadL",
    TRUE~"XXX"
  ),
  cctrt_id = factor(cctrt_id, levels = ord.cctrt_id),
  cctrt_nice = factor(cctrt_nice, levels = ord.cctrt_nice))
  
d <- 
  cents_eukey %>% 
  left_join(cents_cropyields) %>% 
  mutate(estimate = yield_dry_Mgha) %>% 
  mutate(cctrt_nice = case_when(
         cctrt_id == "nocc" ~ "NoCC",
         cctrt_id == "mix_E" ~ "MixE",
         cctrt_id == "mix_M" ~ "MixM",
         cctrt_id == "rad_M" ~ "RadM",
         cctrt_id == "rad_L" ~ "RadL",
         TRUE~"XXX"
       ),
       cctrt_id = factor(cctrt_id, levels = ord.cctrt_id),
       cctrt_nice = factor(cctrt_nice, levels = ord.cctrt_nice),
       crop = case_when(
         crop == "faba bean" ~ "Faba bean (2020)",
         crop == "oat" ~ "Oat (2019)",
         crop == "spring barley" ~ "Spring barley (2018)"
       ),
       crop = factor(crop, levels = ord.crop))

# fig ---------------------------------------------------------------------


em_cc %>% 
  ggplot(aes(cctrt_nice, estimate)) +
  geom_jitter(data = d, aes(cctrt_nice, estimate, fill = crop, shape = crop), 
              width = 0.1, alpha = 0.3) +
  geom_errorbar(aes(x = cctrt_nice, ymin = estimate - std.error, ymax = estimate + std.error),
                width = 0.1, size = 1) +
  geom_point(size = 3) +
  geom_text(aes(x = cctrt_nice, y = estimate + 1.8, label = letter_sig)) +
  scale_y_continuous(limits = c(0, 6)) +
  scale_shape_manual(values = c(22, 21, 24)) +
  scale_fill_manual(values = c(cv3, av3, av4)) +
  theme_bw() +
  #facet_grid(.~crop) +
 labs(x = NULL,
       y = myyieldlab,
      fill = NULL,
      shape = NULL) +
  theme(legend.position = c(0.3,0.15),
        legend.background = element_rect(fill = "transparent"))

ggsave("figs/fig_crop-yields.png",
       width = 4, height = 4)

#--separate crops, have a panel with means
p1 <- 
  em_cc %>% 
  ggplot(aes(cctrt_nice, estimate)) +
  geom_jitter(data = d, aes(cctrt_nice, estimate, fill = crop, shape = crop), 
              width = 0.1, alpha = 1, show.legend = F) +
  # geom_errorbar(aes(x = cctrt_nice, ymin = estimate - std.error, ymax = estimate + std.error),
  #               width = 0.1, size = 1) +
  # geom_point(size = 3) +
  #geom_text(aes(x = cctrt_nice, y = estimate + 1.8, label = letter_sig)) +
  scale_y_continuous(limits = c(0, 6)) +
  scale_shape_manual(values = c(22, 21, 24)) +
  scale_fill_manual(values = c(cv3, av3, av4)) +
  theme_bw() +
  facet_grid(.~crop) +
  labs(x = NULL,
       y = myyieldlab,
       fill = NULL,
       shape = NULL) +
  theme(legend.position = c(0.3,0.15),
        legend.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "transparent"))

p1

p2 <- 
  em_cc %>%
  mutate(tmp.facet = "Marginal Means") %>% 
  ggplot(aes(cctrt_nice, estimate)) +
  # geom_jitter(data = d, aes(cctrt_nice, estimate, fill = crop, shape = crop), 
  #             width = 0.1, alpha = 0.3, show.legend = F) +
  geom_errorbar(aes(x = cctrt_nice, ymin = estimate - std.error, ymax = estimate + std.error),
                width = 0, size = 1) +
  geom_point(size = 2) +
  geom_text(aes(x = cctrt_nice, y = estimate + 1.8, label = letter_sig)) +
  scale_y_continuous(limits = c(0, 6)) +
  scale_shape_manual(values = c(22, 21, 24)) +
  scale_fill_manual(values = c(cv3, av3, av4)) +
  theme_bw() +
  facet_grid(.~tmp.facet) +
  labs(x = NULL,
       y = NULL,
       fill = NULL,
       shape = NULL) +
  theme(legend.position = c(0.3,0.15),
        legend.background = element_rect(fill = "transparent"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

p1 + p2 +  plot_layout(widths = c(3, 1))

ggsave("figs/fig_crop-yields.png",
       width = 8, height = 4)
       