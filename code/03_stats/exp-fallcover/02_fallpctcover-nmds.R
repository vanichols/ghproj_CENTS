# created 6 nov 2024
# purpose: do stats on vegetation cover, community analysis
# notes: 3 subreps make up a single eu
#         based on code from my cover crop paper
# updated 19 feb 2025, working but looks weird. year then cc_trt are drivers, boring

Sys.setLanguage("en")
library(tidyverse)
library(CENTSdata)
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
nmds_res <- metaMDS(mat_dat, 
                    distance = 'bray', 
                    autotransform = F, 
                    expand = F, 
                    k = 3)

#-close to 0.10, pretty reliable
nmds_res$stress

#--are individual points being problematic?
gof <- goodness(object = nmds_res)

plot(nmds_res, 
     display = "sites",
     type = "none")

points(nmds_res, display = "sites",
       cex = 2*gof/mean(gof))
# the left is 2018, the right is 2019
plot(nmds_res)
cc_pdist <- dist(scores(nmds_res, display = 'sites'))

#--run it for individual years?
mat_dat18 <- 
  y1 %>% 
  filter(year == 2018) %>% 
  select(eu, eppo_code, cover_pct) %>%
  pivot_wider(names_from = eppo_code, values_from = cover_pct) %>% 
  replace(is.na(.), 0) %>% 
  column_to_rownames(var = "eu")

#--if you exclude soil, it doesn't converge
nmds_res18 <- metaMDS(mat_dat18, 
                    distance = 'bray', 
                    autotransform = F, 
                    expand = F, 
                    k = 3)

#--oh much better
nmds_res18$stress
plot(nmds_res18)
gof18 <- goodness(object = nmds_res18)

plot(nmds_res18, 
     display = "sites",
     type = "none")

points(nmds_res18, display = "sites",
       cex = 2*gof/mean(gof))

mat_dat19 <- 
  y1 %>% 
  filter(year == 2019) %>% 
  select(eu, eppo_code, cover_pct) %>%
  pivot_wider(names_from = eppo_code, values_from = cover_pct) %>% 
  replace(is.na(.), 0) %>% 
  column_to_rownames(var = "eu")

#--if you exclude soil, it doesn't converge
nmds_res19 <- metaMDS(mat_dat19, 
                      distance = 'bray', 
                      autotransform = F, 
                      expand = F, 
                      k = 3)
nmds_res19$stress
plot(nmds_res19)
gof19 <- goodness(object = nmds_res19)

plot(nmds_res19, 
     display = "sites",
     type = "none")

points(nmds_res19, display = "sites",
       cex = 2*gof/mean(gof))

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
  
# site_scores %>%
#  write_csv("code/04_stats-fallcover/st_nmds-site.csv")

spp_scores  <- 
  as.data.frame(scores(nmds_res, "species")) %>%
  rownames_to_column(., var = "eppo_code")

# spp_scores %>%
#   write_csv("code/04_stats-fallcover/st_nmds-spp.csv")

# Makes polygons for site by treatment
site_hull <- 
  site_scores %>% # dataframe of site scores
  unite("till_straw_cc_year", till_id, straw_id, cctrt_id, year, remove = FALSE) %>%
  group_by(till_straw_cc_year) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

# site_hull %>%
#  write_csv("code/04_stats-fallcover/st_nmds-site-hulls.csv")

site_hull_xtill <- 
  site_scores %>% # dataframe of site scores
  unite("straw_cc_year", straw_id, cctrt_id, year, remove = FALSE) %>%
  group_by(straw_cc_year) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect


# do anova ----------------------------------------------------------------

df_dat18 <- 
  y1 %>% 
  filter(year == 2018) %>% 
  select(eu, eppo_code, cover_pct) %>%
  pivot_wider(names_from = eppo_code, values_from = cover_pct) %>% 
  replace(is.na(.), 0)

dd18 <- 
  df_dat18 %>%
  left_join(y1 %>% 
              select(eu, eu_id, year, date2, subrep) %>% 
              filter(year == 2018) %>% 
              distinct()) %>% 
  left_join(eu) %>% 
  mutate_if(is.character, as.factor) 


#--everything is significnat, cover crop explains a lot more variance than the other two though
adonis2(df_dat18 %>% select(-1) %>% as.matrix() ~ 
          cctrt_id + till_id + straw_id, data = (dd18),
        by = "margin"
)

df_dat19 <- 
  y1 %>% 
  filter(year == 2019) %>% 
  select(eu, eppo_code, cover_pct) %>%
  pivot_wider(names_from = eppo_code, values_from = cover_pct) %>% 
  replace(is.na(.), 0)

dd19 <- 
  df_dat19 %>%
  left_join(y1 %>% 
              select(eu, eu_id, year, date2, subrep) %>% 
              filter(year == 2019) %>% 
              distinct()) %>% 
  left_join(eu) %>% 
  mutate_if(is.character, as.factor) 



adonis2(df_dat19 %>% select(-1) %>% as.matrix() ~ 
          cctrt_id + till_id + straw_id, data = (dd19),
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




# present separate analyses by year ---------------------------------------

site_scores18 <- 
  as.data.frame(scores(nmds_res18, "sites")) %>%
  rownames_to_column(., var = "eu")  %>% 
  left_join(y1 %>% 
              select(eu, eu_id, year, date2, subrep) %>% 
              filter(year == 2018) %>% 
              distinct()) %>% 
  left_join(eu) %>% 
  as_tibble()

site_scores19 <- 
  as.data.frame(scores(nmds_res19, "sites")) %>%
  rownames_to_column(., var = "eu")  %>% 
  left_join(y1 %>% 
              select(eu, eu_id, year, date2, subrep) %>% 
              filter(year == 2019) %>% 
              distinct()) %>% 
  left_join(eu) %>% 
  as_tibble()


spp_scores18  <- 
  as.data.frame(scores(nmds_res18, "species")) %>%
  rownames_to_column(., var = "eppo_code")

spp_scores19  <- 
  as.data.frame(scores(nmds_res19, "species")) %>%
  rownames_to_column(., var = "eppo_code")


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

# do anova ----------------------------------------------------------------
dd18 <-
  df_dat %>%
  left_join(y1 %>%
              select(eu, eu_id, date2, subrep) %>%
              distinct()) %>%
  left_join(eu) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(yearF = as.factor(year(date2)))
# 
# 
# #--everything is significnat, year most of all (3000x), then cc (500x), then straw (14), then till (7)
# adonis2(df_dat %>% select(-1) %>% as.matrix() ~ 
#           cctrt_id + till_id + straw_id + yearF, data = (dd),
#         by = "margin"
# )
# 
# 

# individual years? ---------------------------------
#--just for exploring, if make manu fig it will be in other folder
library(ggrepel)
library(ggpubr)
library(ggh4x)


mylegendtheme <- theme(legend.position = c(0.1, 0.9),
                       legend.justification = c(0,1),
                       legend.background = element_rect(color = "black"))

myaxistexttheme <- theme(axis.text = element_text(size = rel(1.2)),
                         legend.text = element_text(size = rel(1.3)),
                         axis.title = element_text(size = rel(1.3)),
                         strip.text = element_text(size = rel(1.3)))



# #--not sure if necessary...
# site_hull_xtill <- 
#   site_hull %>% 
#   group_by(cctrt_id, year, straw_id) %>% 
#   slice(c(1, n()))

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
library(patchwork)

p1 +p2 


