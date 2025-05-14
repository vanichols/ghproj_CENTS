#--nmds results

library(tidyverse)
library(CENTSdata)
library(patchwork)
library(ggh4x)

rm(list = ls())

source("code/00_color-palettes.R")

# data --------------------------------------------------------------------

#--just for exploring, if make manu fig it will be in other folder
library(ggrepel)
library(ggpubr)
library(ggh4x)


# data --------------------------------------------------------------------

site_scores <- read_csv("code/04_stats-fallcover/st_nmds-site.csv")
site_scores18 <- site_scores %>% filter(year == 2018)
site_scores19 <- site_scores %>% filter(year == 2019)

spp_scores <- read_csv("code/04_stats-fallcover/st_nmds-spp.csv")
spp_scores18 <- spp_scores %>% filter(year == 2018)
spp_scores19 <- spp_scores %>% filter(year == 2019)


site_hull <- read_csv("code/04_stats-fallcover/st_nmds-site-hulls.csv")
site_hull18 <- site_hull %>% filter(year == 2018)
site_hull19 <- site_hull %>% filter(year == 2019)

# Makes polygons for site by treatment
site_hull18 <- 
  site_scores18 %>% # dataframe of site scores
  unite("till_straw_cc", till_id, straw_id, cctrt_id, remove = FALSE) %>%
  group_by(till_straw_cc) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

# Makes polygons for site by treatment
site_hull18_xstraw <- 
  site_scores18 %>% # dataframe of site scores
  unite("till_cc", till_id, cctrt_id, remove = FALSE) %>%
  group_by(till_cc) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

site_hull18_onlycc <- 
  site_scores18 %>% # dataframe of site scores
  #unite("till_cc", till_id, cctrt_id, remove = FALSE) %>%
  group_by(cctrt_id) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

site_hull19 <- 
  site_scores19 %>% # dataframe of site scores
  unite("till_straw_cc", till_id, straw_id, cctrt_id, remove = FALSE) %>%
  group_by(till_straw_cc) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

site_hull19_onlycc <- 
  site_scores19 %>% # dataframe of site scores
  #unite("till_straw_cc", till_id, straw_id, cctrt_id, remove = FALSE) %>%
  group_by(cctrt_id) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect


# fig settings ------------------------------------------------------------

mylegendtheme <- theme(legend.position = c(0.1, 0.9),
                       legend.justification = c(0,1),
                       legend.background = element_rect(color = "black"))

myaxistexttheme <- theme(axis.text = element_text(size = rel(1.2)),
                         legend.text = element_text(size = rel(1.3)),
                         axis.title = element_text(size = rel(1.3)),
                         strip.text = element_text(size = rel(1.3)))



p1 <-
  ggplot() +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  # geom_point(data = site_scores, 
  #            aes(x = NMDS1, 
  #                y = NMDS2, 
  #                color = cctrt_id, shape = till_id), 
  #            size = 3, 
  #            alpha = 0.6) +
  geom_text_repel(data = spp_scores18, 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = eppo_code), 
                  alpha = 0.5) + # Species as text - better!
  geom_polygon(data = site_hull18_onlycc,
               aes(x = NMDS1,
                   y = NMDS2,
                   fill = cctrt_id),
               alpha = 0.3) +
  facet_wrap( ~ year) +
  theme(legend.direction  = "vertical",
        legend.background = element_rect(color = "black"),
        legend.text       = element_text(size = 12),
        legend.title      = element_text(size = 14),
        axis.title        = element_text(size = 14),
        axis.text         = element_text(size = 12))




p2 <- 
  ggplot() +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  # geom_point(data = site_scores, 
  #            aes(x = NMDS1, 
  #                y = NMDS2, 
  #                color = cctrt_id, shape = till_id), 
  #            size = 3, 
  #            alpha = 0.6) +
  geom_text_repel(data = spp_scores19, 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = eppo_code), 
                  alpha = 0.5) + # Species as text - better!
  geom_polygon(data = site_hull19_onlycc,
               aes(x = NMDS1,
                   y = NMDS2,
                   fill = cctrt_id), show.legend = F,
               alpha = 0.3) +
  facet_wrap( ~ year) +
  theme(legend.direction  = "vertical",
        legend.background = element_rect(color = "black"),
        legend.text       = element_text(size = 12),
        legend.title      = element_text(size = 14),
        axis.title        = element_text(size = 14),
        axis.text         = element_text(size = 12))


p1 +p2 
