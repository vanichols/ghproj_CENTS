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
  select(eu, eppo_code, cover_pct) %>%
  mutate(cover_pct = round(cover_pct, 0)) %>% 
  pivot_wider(names_from = eppo_code, values_from = cover_pct) %>%
  replace(is.na(.), 0) #--make NAs into 0s


# analysis, excluding soil----------------------------------------------------------------

mat_dat2 <- 
  df_dat %>% 
  select(-soil) %>% 
  column_to_rownames(var = "eu")


res2 <- 
  mat_dat2 %>%
  mutate(shan2 = diversity(.)) %>%
  mutate(shan_hill2= exp(shan2))

d_res2 <-
  res2 %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  select(eu = rowname, shan2, shan_hill2)

#--data for stats
s <- 
  y1 %>% 
  select(eu_id, eu, subrep, year) %>% 
  distinct() %>% 
  left_join(d_res2) |> 
  left_join(cents_eukey) |> 
  mutate(weayear = paste0("Y", year))

s |> 
  ggplot(aes(shan_hill2, shan2)) +
  geom_point()


# models ------------------------------------------------------------------


m1 <- glmmTMB(shan2 ~ till_id * cctrt_id * straw_id * weayear +
                (1|block_id/straw_id/till_id/cctrt_id),
              REML = T, 
              data=s)


m1a <- glmmTMB(shan2 ~ till_id * cctrt_id * straw_id * weayear +
                (1|block_id/straw_id/till_id/cctrt_id),
              #REML = T, 
              data=s)


m2 <- glmmTMB(shan_hill2 ~ till_id * cctrt_id * straw_id * weayear +
                (1|block_id/straw_id/till_id/cctrt_id),
              REML = T, 
              data=s)

#--lots of Nas...
summary(m1)
sim_rest1 <- simulateResiduals(m1)
#--doesn't look great
plot(sim_rest1)

summary(m2)
sim_rest2 <- simulateResiduals(m2)
#--doesn't look great
plot(sim_rest2)

joint_tests(m1)
