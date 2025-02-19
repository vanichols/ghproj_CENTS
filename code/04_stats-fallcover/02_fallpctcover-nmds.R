# created 6 nov 2024
# purpose: do stats on vegetation cover, community analysis
# notes: 3 subreps make up a single eu
#         based on code from my cover crop paper
# updated 19 feb 2025, working but looks weird. year then cc_trt are drivers, boring

Sys.setLanguage("en")
library(tidyverse)
library(lme4)
library(lmerTest)
library(CENTSdata)
library(broom)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(car)
library(vegan)


rm(list = ls())

# data --------------------------------------------------------------------

w <- cents_wea
eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_fallpctcover)

y1 <- 
  y %>% 
  mutate(
    year = lubridate::year(date2),
    eu = paste(eu_id, subrep, year, sep = "_")) 

df_dat <- 
  y1 %>% 
  select(eu, eppo_code, cover_pct) %>%
  #filter(eppo_code!= "soil") %>% 
  pivot_wider(names_from = eppo_code, values_from = cover_pct) %>% 
  replace(is.na(.), 0)


# analysis ----------------------------------------------------------------

mat_dat <- 
  df_dat %>% 
  column_to_rownames(var = "eu")

#--if you exclude soil, it doesn't converge
nmds_res <- metaMDS(mat_dat, distance = 'bray', autotransform = F, expand = F)
# the left is 2018, the right is 2019
plot(nmds_res)
cc_pdist <- dist(scores(nmds_res, display = 'sites'))

#--need help knowing what to report about this fit

site_scores <- 
  as.data.frame(scores(nmds_res, "sites")) %>%
  rownames_to_column(., var = "eu")  %>% 
  left_join(y1 %>% 
              select(eu, eu_id, date2, subrep) %>% 
              distinct()) %>% 
  left_join(eu) %>% 
    as_tibble() %>% 
  mutate(year = year(date2))
  
site_scores %>%
 write_csv("code/04_stats-fallcover/st_nmds-site.csv")

spp_scores  <- 
  as.data.frame(scores(nmds_res, "species")) %>%
  rownames_to_column(., var = "eppo_code")

spp_scores %>%
  write_csv("code/04_stats-fallcover/st_nmds-spp.csv")

# Makes polygons for site by treatment
site_hull <- 
  site_scores %>% # dataframe of site scores
  unite("till_straw_cc_year", till_id, straw_id, cctrt_id, year, remove = FALSE) %>%
  group_by(till_straw_cc_year) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

site_hull %>%
 write_csv("code/04_stats-fallcover/st_nmds-site-hulls.csv")

site_hull_xtill <- 
  site_scores %>% # dataframe of site scores
  unite("straw_cc_year", straw_id, cctrt_id, year, remove = FALSE) %>%
  group_by(straw_cc_year) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect


# do anova ----------------------------------------------------------------
dd <- 
  df_dat %>%
  left_join(y1 %>% 
              select(eu, eu_id, date2, subrep) %>% 
              distinct()) %>% 
  left_join(eu) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(yearF = as.factor(year(date2))) 


#--everything is significnat, year most of all (3000x), then cc (500x), then straw (14), then till (7)
adonis2(df_dat %>% select(-1) %>% as.matrix() ~ 
          cctrt_id + till_id + straw_id + yearF, data = (dd),
        by = "margin"
)



# individual years? ---------------------------------
# figure ------------------------------------------------------------------
#--just for exploring, if make manu fig it will be in other folder
library(ggrepel)
library(ggpubr)
library(ggh4x)


# fig settings ------------------------------------------------------------

mylegendtheme <- theme(legend.position = c(0.1, 0.9),
                       legend.justification = c(0,1),
                       legend.background = element_rect(color = "black"))

myaxistexttheme <- theme(axis.text = element_text(size = rel(1.2)),
                         legend.text = element_text(size = rel(1.3)),
                         axis.title = element_text(size = rel(1.3)),
                         strip.text = element_text(size = rel(1.3)))



#--not sure if necessary...
site_hull_xtill <- 
  site_hull %>% 
  group_by(cctrt_id, year, straw_id) %>% 
  slice(c(1, n()))


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


ggsave("02_make-figs/manu/fig3.jpg", width = 8.3, height = 5.7)
