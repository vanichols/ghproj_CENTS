#--nmds results
#--creatd 1 sept 2025

library(tidyverse)
library(CENTSdata)
library(patchwork)
library(ggh4x)
library(ggrepel)
library(ggpubr)

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

# data --------------------------------------------------------------------

site_scores <- 
  read_csv("data/stats/stats_nmds-site.csv") %>% 
  MakeNiceLabels(.)

site_scores18 <- site_scores %>% filter(year == 2018)
site_scores19 <- site_scores %>% filter(year == 2019)

spp_scores <- 
  read_csv("data/stats/stats_nmds-spp.csv") %>% 
  mutate(eppo_code = str_to_upper(eppo_code))
# spp_scores18 <- spp_scores %>% filter(year == 2018)
# spp_scores19 <- spp_scores %>% filter(year == 2019)

site_hull <- 
  read_csv("data/stats/stats_nmds-site-hulls.csv") %>% 
  MakeNiceLabels(.)
site_hull18 <- site_hull %>% filter(year == 2018)
site_hull19 <- site_hull %>% filter(year == 2019)

# Makes polygons for site by treatment
# site_hull18 <- 
#   site_scores18 %>% # dataframe of site scores
#   unite("till_straw_cc", till_id, straw_id, cctrt_id, remove = FALSE) %>%
#   group_by(till_straw_cc) %>% # grouping variables: farm AND treatmnet
#   slice(chull(NMDS1, NMDS2)) # points that polygons will connect

# Makes polygons for site by treatment
# site_hull18_xstraw <- 
#   site_scores18 %>% # dataframe of site scores
#   unite("till_cc", till_id, cctrt_id, remove = FALSE) %>%
#   group_by(till_cc) %>% # grouping variables: farm AND treatmnet
#   slice(chull(NMDS1, NMDS2)) # points that polygons will connect

site_hull18_onlycc <- 
  site_scores18 %>% # dataframe of site scores
  group_by(cctrt_nice) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

# site_hull19 <- 
#   site_scores19 %>% # dataframe of site scores
#   unite("till_straw_cc", till_id, straw_id, cctrt_id, remove = FALSE) %>%
#   group_by(till_straw_cc) %>% # grouping variables: farm AND treatmnet
#   slice(chull(NMDS1, NMDS2)) # points that polygons will connect

site_hull19_onlycc <- 
  site_scores19 %>% # dataframe of site scores
  group_by(cctrt_nice) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

site_hull_onlycc <- 
  site_hull18_onlycc %>% 
  bind_rows(site_hull19_onlycc)

# site_hull18_xtill <- 
#   site_hull18 %>% 
#   group_by(cctrt_id, year) %>% 
#   slice(c(1, n()))

# site_hull19_xtill <- 
#   site_hull19 %>% 
#   group_by(cctrt_id, year) %>% 
#   slice(c(1, n()))
# 
# site_hull_xtill <- 
#   site_hull18_xtill %>% 
#   bind_rows(site_hull19_xtill)

####--fig--####

th1 <-   theme(axis.ticks.y = element_blank(),
               axis.text.x = element_text(angle = 45, hjust = 1),
               legend.position = "top",
               strip.background.x = element_rect(fill = "white", 
                                                 color = "black"),
               strip.text.x = element_text(size = rel(1.3)),
               panel.border = element_blank())

ggplot() +
  geom_text_repel(data = spp_scores, 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = eppo_code), 
                  alpha = 0.5) + # Species as text - better!
  # geom_path(data = site_hull_onlycc,
  #              aes(x = NMDS1,
  #                  y = NMDS2,
  #                  color = cctrt_id),
  #              alpha = 0.3, fill = "transparent") +
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
  scale_color_manual(values = c( 
                       "NoCC" = hue_nocc,
                     "MixE" = hue_mixe,
                     "MixM" = hue_mixm,
                     "RadM" = hue_radm,
                     "RadL" = hue_radl)) +
  facet_nested(. ~ year) +
  labs(shape = "Tillage system",
       color = "Cover crop system") +
  theme(legend.direction  = "horizontal",
        legend.position = "top",
        legend.background = element_rect(color = "black"),
        legend.text       = element_text(size = 12),
        legend.title      = element_text(size = 14),
        axis.title        = element_text(size = 14),
        axis.text         = element_text(size = 12)) +
  th1


