# created 6 nov 2024
# purpose: do stats on vegetation cover, community analysis
# notes: 3 subreps make up a single eu
#         based on code from my cover crop paper
# updated 19 feb 2025, working but looks weird. year then cc_trt are drivers, boring
# updated 9 april, sep analyses by year, write res for fig
#--try reassigning barley and oats to be just 'cash crop volunteer'

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
    eu = paste(eu_id, subrep, year, sep = "_")) |> 
  mutate(eppo_code = ifelse(cover_cat2 == "volunteer", "volunteer", eppo_code))

df_dat <- 
  y1 |> 
  select(eu, eppo_code, cover_pct)  |> 
  #filter(eppo_code!= "soil") %>% 
  pivot_wider(names_from = eppo_code, values_from = cover_pct) %>% 
  replace(is.na(.), 0)


# analysis ----------------------------------------------------------------


mat_dat <- 
  y1 %>% 
  select(eu, eppo_code, cover_pct) %>%
  pivot_wider(names_from = eppo_code, values_from = cover_pct) %>% 
  replace(is.na(.), 0) %>% 
  column_to_rownames(var = "eu")

nmds_res <- metaMDS(mat_dat, 
                    distance = 'bray', 
                    autotransform = F, 
                    expand = F, 
                    k = 3)

#--stress of <0.2 is considered acceptable
nmds_res$stress
plot(nmds_res)
gof <- goodness(object = nmds_res)

plot(nmds_res, 
     display = "sites",
     type = "none")

points(nmds_res, display = "sites",
       cex = 2*gof/mean(gof))


# do anova ----------------------------------------------------------------

dd <- 
  df_dat %>%
  left_join(y1 %>% 
              select(eu, eu_id, year, date2, subrep) %>% 
              distinct()) %>% 
  left_join(eu) %>% 
  mutate(yearF = paste0("Y", year)) |> 
  mutate_if(is.character, as.factor) 


aov <- 
  adonis2(df_dat %>% select(-1) %>% as.matrix() ~ 
          cctrt_id + till_id + straw_id + yearF, data = (dd),
        by = "margin"
)

#--same results
tidy(aov)  |> 
  write_xlsx("data/stats/supp_tables/fallcommunity-anova-onemodel.xlsx")

42/76
6/76

# get site scores, etc ----------------------------------------------------

site_scores <- 
  as.data.frame(scores(nmds_res, "sites")) %>%
  rownames_to_column(., var = "eu")  %>% 
  left_join(y1 %>% 
              select(eu, eu_id, date2, subrep) %>% 
              distinct()) %>% 
  left_join(eu) %>% 
  as_tibble() %>% 
  mutate(year = year(date2))

site_scores |>  
  write_csv("data/stats/figs_emmeans/nmds-fig3-sitescores-onemodel.csv")

spp_scores  <- 
  as.data.frame(scores(nmds_res, "species")) %>%
  rownames_to_column(., var = "eppo_code") 

spp_scores |>  
  write_csv("data/stats/figs_emmeans/nmds-fig3-speciesscores-onemodel.csv")


# Makes polygons for site by treatment
site_hull <- 
  site_scores %>% # dataframe of site scores
  unite("till_straw_cc_year", till_id, straw_id, cctrt_id, year, remove = FALSE) %>%
  group_by(till_straw_cc_year) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

site_hull|> 
  write_csv("data/stats/figs_emmeans/nmds-fig3-hulls-onemodel.csv")



# practice fig ------------------------------------------------------------


site_hull_onlycc <- 
  site_scores %>% # dataframe of site scores
  group_by(cctrt_id) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

####--fig--####

th1 <-   theme(axis.ticks.y = element_blank(),
               axis.text.x = element_text(angle = 45, hjust = 1),
               legend.position = "top",
               strip.background.x = element_rect(fill = "white", 
                                                 color = "black"),
               strip.text.x = element_text(size = rel(1.3)),
               panel.border = element_blank())

  ggplot() +
  geom_polygon(data = site_hull_onlycc,
               aes(x = NMDS1,
                   y = NMDS2,
                   color = cctrt_id),
               alpha = 0.3, fill = "transparent") +
  geom_point(data = site_scores,
             aes(x = NMDS1,
                 y = NMDS2,
                 color = cctrt_id, shape = till_id),
             size = 3,
             alpha = 0.6) +
  geom_text_repel(data = spp_scores, 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = eppo_code), 
                  alpha = 0.5) + # Species as text - better!
  # scale_color_manual(values = c( 
  #   "NoCC" = hue_nocc,
  #   "MixE" = hue_mixe,
  #   "MixM" = hue_mixm,
  #   "RadM" = hue_radm,
  #   "RadL" = hue_radl)) +
  #facet_nested(. ~ year) +
  # labs(shape = "Tillage",
  #      color = "Cover crop") +
  theme(legend.direction  = "horizontal",
        legend.position = "top",
        legend.background = element_rect(color = "black"),
        legend.text       = element_text(size = 12),
        legend.title      = element_text(size = 14),
        axis.title        = element_text(size = 14),
        axis.text         = element_text(size = 12)) +
  th1


