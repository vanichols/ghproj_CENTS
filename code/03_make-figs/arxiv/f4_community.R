#--nmds results

library(tidyverse)
library(CENTSdata)
library(patchwork)
library(ggh4x)
library(ggrepel)
library(ggpubr)

rm(list = ls())

source("code/00_color-palettes.R")

# data --------------------------------------------------------------------

site_scores <- read_csv("data/stats/stats_nmds-site.csv")
site_scores18 <- site_scores %>% filter(year == 2018)
site_scores19 <- site_scores %>% filter(year == 2019)

spp_scores <- read_csv("data/stats/stats_nmds-spp.csv")
spp_scores18 <- spp_scores %>% filter(year == 2018)
spp_scores19 <- spp_scores %>% filter(year == 2019)


site_hull <- read_csv("data/stats/stats_nmds-site-hulls.csv")
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

site_hull18_xtill <- 
  site_hull18 %>% 
  group_by(cctrt_id, year, straw_id) %>% 
  slice(c(1, n()))

site_hull19_xtill <- 
  site_hull19 %>% 
  group_by(cctrt_id, year, straw_id) %>% 
  slice(c(1, n()))

# fig settings ------------------------------------------------------------

mylegendtheme <- theme(legend.position.inside = c(0.1, 0.9),
                       legend.justification = c(0,1),
                       legend.background = element_rect(color = "black"))

myaxistexttheme <- theme(axis.text = element_text(size = rel(1.2)),
                         legend.text = element_text(size = rel(1.3)),
                         axis.title = element_text(size = rel(1.3)),
                         strip.text = element_text(size = rel(1.3)))



# maybe -------------------------------------------------------------------


ggplot() +
  # geom_point(data = site_scores, 
  #            aes(x = NMDS1, 
  #                y = NMDS2, 
  #                color = cctrt_id, shape = till_id), 
  #            size = 3, 
  #            alpha = 0.6) +
  geom_text_repel(data = spp_scores, 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = eppo_code), 
                  alpha = 0.5) + # Species as text - better!
  geom_polygon(data = site_hull_xtill,
               aes(x = NMDS1,
                   y = NMDS2,
                   fill = cctrt_id),
               alpha = 0.3) +
  facet_nested(straw_id ~ year) +
  theme(legend.direction  = "vertical",
        legend.background = element_rect(color = "black"),
        legend.text       = element_text(size = 12),
        legend.title      = element_text(size = 14),
        axis.title        = element_text(size = 14),
        axis.text         = element_text(size = 12))



#nmds3 <- 
ggplot() +
  #--grasses
  geom_text_repel(data = spp_scores1, 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = eppo_code),
                  fontface = "italic",
                  alpha = 0.5, 
                  color = "gray70") + # Species as text - better!
  geom_polygon(data = site_hull1, 
               aes(x = NMDS1, 
                   y = NMDS2, 
                   fill = cctrt_id),
               alpha = 0.3) + 
  geom_path(data = site_hull1,
            aes(x = NMDS1, 
                y = NMDS2,
                group = interaction(cctrt_id, till_id, straw_id),
                linetype = cctrt_id
            ),
            na.rm = TRUE,
            color = "gray40",
            size = 1) +
  geom_path(data = site_hull2, 
            aes(x = sites.NMDS1, 
                y = sites.NMDS2,
                group = interaction(cctrt_id, till_id, straw_id),
                linetype = cctrt_id
            ),
            na.rm = TRUE,
            color = "gray40",
            size = 0.9) +
  facet_wrap(.~year) 






#geom_hline(yintercept = 0, lty = 2) +
#geom_vline(xintercept = 0, lty = 2) +
# -- the following stuff is for aesthetic purposes --
scale_color_manual(values = c(p_pink, p_green, p_blue, p_orange, p_purp)) +
  scale_fill_manual(values = c(p_yellow, p_yellow,
                               p_purp, p_purp,
                               p_green, p_green,
                               p_blue, p_blue,
                               p_orange, p_orange)) +
  labs(color = "Site",
       linetype = "Cover Crop Treatment")+
  guides(fill = FALSE,
         shape = F)+
  theme_minimal() + 
  theme(legend.direction  = "vertical",
        legend.background = element_rect(color = "black"),
        legend.justification = c(1, 0),
        legend.position = c(0.95, 0.15),
        #legend.text       = element_text(size = 12),
        #legend.title      = element_text(size = 14),
        #axis.title        = element_text(size = 14),
        #axis.text         = element_text(size = 12),
        strip.text = element_text(face = "bold", size = rel(1.2)),
        #legend.key.size = unit(0.8, "lines"),
        legend.title = element_text(size = rel(1), face = "bold"),
        legend.text = element_text(size = rel(1)))



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
