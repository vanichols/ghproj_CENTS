#--nmds results
#--creatd 1 sept 2025

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
MakeNiceLabels <- function(dat){
  d <- dat %>% 
    mutate(cctrt_nice = case_when(
    cctrt_id == "nocc" ~ "NoCC",
    cctrt_id == "mix_E" ~ "MixE",
    cctrt_id == "mix_M" ~ "MixM",
    cctrt_id == "rad_M" ~ "RadM",
    cctrt_id == "rad_L" ~ "RadL",
    TRUE~"XXX"
  ),
  cctrt_id = factor(cctrt_id, levels = ord.cctrt_id),
  cctrt_nice = factor(cctrt_nice, levels = ord.cctrt_niceS)) %>% 
    #--make nice till
    mutate(
      till_id = factor(till_id, levels = ord.till_id),
      till_nice = case_when(
        till_id == "notill" ~ "No-till",
        till_id == "inversion" ~ "Inv",
        till_id == "surface" ~ "Surf",
        TRUE ~ "XXX"
      ),
      till_nice = factor(till_nice, levels = ord.till_nice))
  return(d)
}

# 1. nmds fig --------------------------------------------------------------------

site_scores <- 
  read_csv("data/stats/stats_nmds-site.csv") %>% 
  MakeNiceLabels(.)

site_scores18 <- site_scores %>% filter(year == 2018)
site_scores19 <- site_scores %>% filter(year == 2019)

spp_scores <- 
  read_csv("data/stats/stats_nmds-spp.csv") %>% 
  mutate(eppo_code = str_to_upper(eppo_code))

site_hull <- 
  read_csv("data/stats/stats_nmds-site-hulls.csv") %>% 
  MakeNiceLabels(.)
site_hull18 <- site_hull %>% filter(year == 2018)
site_hull19 <- site_hull %>% filter(year == 2019)

site_hull18_onlycc <- 
  site_scores18 %>% # dataframe of site scores
  group_by(cctrt_nice) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

site_hull19_onlycc <- 
  site_scores19 %>% # dataframe of site scores
  group_by(cctrt_nice) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

site_hull_onlycc <- 
  site_hull18_onlycc %>% 
  bind_rows(site_hull19_onlycc)

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
                 color = cctrt_nice, shape = till_nice),
             size = 3,
             alpha = 0.6) +
  geom_text_repel(data = spp_scores, 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = eppo_code), 
                  alpha = 0.5) + # Species as text - better!
  scale_color_manual(values = c( 
                       "NoCC" = hue_nocc,
                     "MixE" = hue_mixe,
                     "MixM" = hue_mixm,
                     "RadM" = hue_radm,
                     "RadL" = hue_radl)) +
  facet_nested(. ~ year) +
  labs(shape = "Tillage",
       color = "Cover crop") +
  theme(legend.direction  = "horizontal",
        legend.position = "top",
        legend.background = element_rect(color = "black"),
        legend.text       = element_text(size = 12),
        legend.title      = element_text(size = 14),
        axis.title        = element_text(size = 14),
        axis.text         = element_text(size = 12)) +
  th1


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
  group_by(year, till_id, cctrt_id) %>% 
  summarise(potval = mean(potval, na.rm = T)) %>% 
  ungroup() %>% 
  MakeNiceLabels(.)

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
  facet_grid(.  ~ year) +
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

p1 /p2 + plot_layout(heights = c(1, 1))

ggsave("figs/fig3_comm-and-potval.png",
       width = 10, height = 10)
