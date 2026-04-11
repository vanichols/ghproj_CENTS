# created 6 nov 2024
# purpose: do stats on vegetation cover, community analysis
# notes: 3 subreps make up a single eu
#         based on code from my cover crop paper
# updated 19 feb 2025, working but looks weird. year then cc_trt are drivers, boring
# updated 9 april, sep analyses by year, write res for fig

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

#--run it for individual years

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

#--oh much better stress, but best solution isn't repeated in this year
nmds_res18$stress
plot(nmds_res18)
gof18 <- goodness(object = nmds_res18)

plot(nmds_res18, 
     display = "sites",
     type = "none")

points(nmds_res18, display = "sites",
       cex = 2*gof18/mean(gof18))

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
#--good convergence
nmds_res19$stress
plot(nmds_res19)
gof19 <- goodness(object = nmds_res19)

plot(nmds_res19, 
     display = "sites",
     type = "none")

points(nmds_res19, display = "sites",
       cex = 2*gof19/mean(gof19))


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


#--everything is significnat, year most of all (3000x), then cc (500x), then straw (14), then till (7)

#--separate by year
adonis2(df_dat18 %>% select(-1) %>% as.matrix() ~ 
          cctrt_id + till_id + straw_id, data = (dd18),
        by = "margin"
)



# get site scores, etc ----------------------------------------------------

site_scores18 <- 
  as.data.frame(scores(nmds_res18, "sites")) %>%
  rownames_to_column(., var = "eu")  %>% 
  left_join(y1 %>% 
              select(eu, eu_id, date2, subrep) %>% 
              distinct()) %>% 
  left_join(eu) %>% 
  as_tibble() %>% 
  mutate(year = year(date2))

site_scores19 <- 
  as.data.frame(scores(nmds_res19, "sites")) %>%
  rownames_to_column(., var = "eu")  %>% 
  left_join(y1 %>% 
              select(eu, eu_id, date2, subrep) %>% 
              distinct()) %>% 
  left_join(eu) %>% 
  as_tibble() %>% 
  mutate(year = year(date2))


site_scores18 %>%
  bind_rows(site_scores19) %>% 
  write_csv("code/04_stats-fallcover/st_nmds-site.csv")

spp_scores18  <- 
  as.data.frame(scores(nmds_res18, "species")) %>%
  rownames_to_column(., var = "eppo_code")%>% 
  mutate(year = 2018)

spp_scores19  <- 
  as.data.frame(scores(nmds_res19, "species")) %>%
  rownames_to_column(., var = "eppo_code")%>% 
  mutate(year = 2019)

spp_scores18 %>%
  bind_rows(spp_scores19) %>% 
  write_csv("code/04_stats-fallcover/st_nmds-spp.csv")

# Makes polygons for site by treatment
site_hull18 <- 
  site_scores18 %>% # dataframe of site scores
  unite("till_straw_cc_year", till_id, straw_id, cctrt_id, year, remove = FALSE) %>%
  group_by(till_straw_cc_year) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

site_hull19 <- 
  site_scores19 %>% # dataframe of site scores
  unite("till_straw_cc_year", till_id, straw_id, cctrt_id, year, remove = FALSE) %>%
  group_by(till_straw_cc_year) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

site_hull18 %>%
  bind_rows(site_hull19) %>% 
  write_csv("code/04_stats-fallcover/st_nmds-site-hulls.csv")


