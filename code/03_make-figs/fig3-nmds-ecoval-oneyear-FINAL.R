#--nmds results
#--creatd 1 sept 2025
#--cleaned up 16 april 2026

library(tidyverse)
library(CENTSdata)
library(patchwork)
library(ggh4x)
library(ggrepel)
library(ggpubr)
library(ggpattern)

rm(list = ls())

source("code/00_color-palettes.R")
theme_set(theme_bw())

# 1. nmds fig --------------------------------------------------------------------

site_scores <- 
  read_csv("data/stats/figs_emmeans/nmds-fig3-sitescores-onemodel.csv") %>% 
  MakeNiceLabels(.)


spp_scores <- 
  read_csv("data/stats/figs_emmeans/nmds-fig3-speciesscores-onemodel.csv") |> 
  mutate(eppo_code = str_to_upper(eppo_code))

site_hull <- 
  read_csv("data/stats/figs_emmeans/nmds-fig3-hulls-onemodel.csv") %>% 
  MakeNiceLabels(.)


site_hull_onlycc <- 
  site_scores %>% # dataframe of site scores
  group_by(cctrt_nice) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect


####--fig--####

th1 <-   theme(axis.ticks.y = element_blank(),
               axis.text.x = element_text(angle = 45, hjust = 1),
               legend.position = "top",
               strip.background.x = element_rect(fill = "white", 
                                                 color = "black"),
               strip.text.x = element_text(size = rel(1.3)),
               panel.border = element_blank())

p1 <- 
  ggplot() +
  geom_polygon(data = site_hull_onlycc,
            aes(x = NMDS1,
                y = NMDS2,
                color = cctrt_nice),
            alpha = 0.3, fill = "transparent") +
  geom_point(data = site_scores,
             aes(x = NMDS1,
                 y = NMDS2,
                 fill = cctrt_nice, shape = cctrt_nice, color = cctrt_nice),
             size = 3,
             #color = "transparent",
             alpha = 0.5) +
  geom_text_repel(data = spp_scores, 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = eppo_code), 
                  max.overlaps = 20,
                  #alpha = 0.5
                  ) + # Species as text - better!
  scale_fill_manual(values = c( 
                       "NoCC" = hue_nocc,
                     "MixE" = hue_mixe,
                     "MixM" = hue_mixm,
                     "RadM" = hue_radm,
                     "RadL" = hue_radl)) +
  scale_color_manual(values = c( 
    "NoCC" = hue_nocc,
    "MixE" = hue_mixe,
    "MixM" = hue_mixm,
    "RadM" = hue_radm,
    "RadL" = hue_radl)) +
  scale_shape_manual(values = c( 
    "NoCC" = 21,
    "MixE" = 22,
    "MixM" = 23,
    "RadM" = 24,
    "RadL" = 25)) +
  labs(color = "Cover crop",
       fill = "Cover crop",
       shape = "Cover crop") +
  theme(legend.direction  = "horizontal",
        legend.position = "top",
        legend.background = element_rect(color = "black"),
        legend.text       = element_text(size = 12),
        legend.title      = element_text(size = 14),
        axis.title        = element_text(size = 14),
        axis.text         = element_text(size = 12)) +
  th1

p1

# 2. species value --------------------------------------------------------

#--number of species found
# d2z <- 
#   read_csv("data/tidy_nuspecies.csv") %>% 
#   left_join(cents_eukey) %>% 
#   group_by(year, till_id, cctrt_id) %>% 
#   summarise(nu_sp = mean(nu_sp, na.rm = T)) %>% 
#   ungroup() %>% 
#   MakeNiceLabels(.)


d2a <- read_csv("data/tidy_communityvalue.csv")

#--means
d2 <- 
  d2a %>% 
  left_join(cents_eukey) %>% 
  group_by(till_id, cctrt_id) %>% 
  summarise(potval = mean(potval, na.rm = T)) %>% 
  ungroup() %>% 
  MakeNiceLabels2(.)

p2 <- 
  d2 %>% 
  ggplot(aes(cctrt_nice, potval)) +
  geom_col(aes(fill = cctrt_nice,
                       alpha = till_nice),
           position = "dodge",
           color = "black") +
  # geom_text(data = d2z,
  #           aes(cctrt_nice, y = -1, label = nu_sp)) +
  scale_fill_manual(values = c( 
    "NoCC" = hue_nocc,
    "MixE" = hue_mixe,
    "MixM" = hue_mixm,
    "RadM" = hue_radm,
    "RadL" = hue_radl)) +
  guides(fill = "none") +
  labs(alpha = "Tillage system",
       y = "Potential ecosystem value\n(unitless)",
       x = "Cover crop") +
  theme(legend.direction  = "horizontal",
        legend.position = "top",
        legend.background = element_rect(color = "black"),
        legend.text       = element_text(size = 12),
        legend.title      = element_text(size = 14),
        axis.title        = element_text(size = 14),
        axis.text         = element_text(size = 12)) +
  th1



p2

# 3. combine --------------------------------------------------------------

p1 + p2 + plot_layout(widths = c(1, 1))

ggsave("figs/fig3_comm-and-potval-onemodel.png",
       width = 12, height = 6)


# 4. alternative calculation w/rapsr 0 ------------------------------------

#--means
d2alt <- 
  d2a %>% 
  left_join(cents_eukey) %>% 
  group_by(year, till_id, cctrt_id) %>% 
  summarise(potval = mean(potval_0rapsr, na.rm = T)) %>% 
  ungroup() %>% 
  MakeNiceLabels(.)

p2orig <-
  p2 +
  labs(title = "Scenario where RASPR value is included",
       x = NULL)

p2alt <- 
  d2alt %>% 
  ggplot(aes(cctrt_nice, potval)) +
  geom_col(aes(fill = cctrt_nice,
               alpha = till_nice),
           position = "dodge",
           color = "black") +
  # geom_text(data = d2z,
  #           aes(cctrt_nice, y = -1, label = nu_sp)) +
  scale_fill_manual(values = c( 
    "NoCC" = hue_nocc,
    "MixE" = hue_mixe,
    "MixM" = hue_mixm,
    "RadM" = hue_radm,
    "RadL" = hue_radl)) +
  guides(fill = "none") +
  facet_grid(.  ~ year) +
  labs(alpha = "Tillage system",
       y = "Potential ecosystem value\n(unitless)",
       x = NULL,
       title = "Scenario where RAPSR value is set to 0",
       caption = "Note different y-axis scales") +
  theme(legend.direction  = "horizontal",
        legend.position = "top",
        legend.background = element_rect(color = "black"),
        legend.text       = element_text(size = 12),
        legend.title      = element_text(size = 14),
        axis.title        = element_text(size = 14),
        axis.text         = element_text(size = 12)) +
  th1

p2orig / p2alt + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("figs/sfig_community-value-sensitivity.png",
       height = 10, width = 10)
