# created 6 nov 2024
# purpose: do stats on vegetation cover, community analysis
# notes: 3 subreps make up a single eu
#         based on code from my cover crop paper

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
  select(eu, cover_type, cover_pct) %>% 
  pivot_wider(names_from = cover_type, values_from = cover_pct)


# analysis ----------------------------------------------------------------

mat_dat <- 
  df_dat %>% 
  column_to_rownames(var = "eu")

nmds_res <- metaMDS(mat_dat, distance = 'bray', autotransform = F, expand = F)
#doesn't converge, why?
plot(nmds_res)
cc_pdist <- dist(scores(nmds_res, display = 'sites'))

#--need help knowing what to report about this fit

site_scores <- 
  as.data.frame(scores(nmds_res)) %>%
  rownames_to_column() %>% 
  separate(rowname, into = c("site", "sys_trt", "cc_trt", "blockID", "rep"), remove = F) %>% 
  rename(site_sys = rowname) %>% 
  unite("site_sys", site, sys_trt, remove = F)

site_scores %>% 
  write_csv("01_stats-nmds/st_nmds-site.csv")

spp_scores  <- 
  as.data.frame(scores(nmds_res, "species")) %>%
  rownames_to_column(., var = "speciesID")

spp_scores %>% 
  write_csv("01_stats-nmds/st_nmds-spp.csv")

# Makes polygons for site by treatment
site_hull <- 
  site_scores %>% # dataframe of site scores
  unite("site_sys_trt", site_sys, blockID, cc_trt, remove = FALSE) %>%
  group_by(site_sys_trt) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

site_hull %>% 
  write_csv("01_stats-nmds/st_nmds-site-hulls.csv")



# do anova ----------------------------------------------------------------


#--on all sites, duh site is sig, cc is not at all
adonis2(df_dat %>% select(-1) %>% as.matrix() ~ 
          site_sys + cc_trt, data = (df_dat %>% 
                                       separate(eu, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>%
                                       unite(site, crop_sys, field, col = "site_sys", remove = F) %>% 
                                       mutate_if(is.character, as.factor)),
        by = "margin"
)

#--what is the %?
5.0809/12.0426 #--site_sys
0.0818/12.0426 #--cc_trt


# individual sites, where cc-ing was sig? ---------------------------------

#--funcke
adonis2(df_dat %>%
          separate(eu, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
          filter(site == "East") %>%  
          select(-(site:rep)) %>% 
          as.matrix() ~ 
          cc_trt, data = (df_dat %>% 
                            separate(eu, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
                            filter(site == "East") %>% 
                            mutate_if(is.character, as.factor)),
        by = "margin"
)


#--silage
adonis2(df_dat %>%
          separate(eu, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
          filter(crop_sys == "silage") %>%  
          select(-(site:rep)) %>% 
          as.matrix() ~ 
          cc_trt, data = (df_dat %>% 
                            separate(eu, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
                            filter(crop_sys == "silage") %>% 
                            mutate_if(is.character, as.factor))
)

#--central grain maize
adonis2(df_dat %>%
          separate(eu, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
          filter(site == "Central",
                 crop_sys == "grain",
                 field == "B42") %>%  
          select(-(site:rep)) %>% 
          as.matrix() ~ 
          cc_trt, data = (df_dat %>% 
                            separate(eu, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
                            filter(site == "Central",
                                   crop_sys == "grain",
                                   field == "B42") %>% 
                            mutate_if(is.character, as.factor))
)


