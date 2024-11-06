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
  mutate(cover_pct = round(cover_pct, 0)) %>% 
  pivot_wider(names_from = cover_type, values_from = cover_pct)


# analysis, including soil----------------------------------------------------------------

mat_dat <- 
  df_dat %>% 
  column_to_rownames(var = "eu")


res <- 
  mat_dat %>%
  mutate(shan = diversity(.)) %>%
  mutate(shan_hill= exp(shan))

d_res <-
  res %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  select(eu = rowname, shan, shan_hill)



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



# combine them to look at -------------------------------------------------

d_res3 <- 
  d_res %>% 
  left_join(d_res2)

d_res4 <- 
  y1 %>% 
  select(eu_id, eu, subrep, year) %>% 
  distinct() %>% 
  left_join(d_res3)



# viz ---------------------------------------------------------------------

#--including soil
d_res4 %>% 
  left_join(cents_eukey) %>% 
  ggplot(aes(till_id, shan)) +
  geom_jitter(aes(color = cctrt_id)) +
  facet_grid(.~straw_id+year)

#--excluding soil
d_res4 %>% 
  left_join(cents_eukey) %>% 
  ggplot(aes(till_id, shan2)) +
  geom_jitter(aes(color = cctrt_id)) +
  facet_grid(.~straw_id+year)

#--confused how to have a diversity of 0

e1 <- 
  df_dat %>% 
  left_join(y1) %>% 
  left_join(cents_eukey) %>% 
  mutate(trt_sum = paste(till_id, straw_id, cctrt_id)) %>% 
  select(year, till_id, straw_id, cctrt_id, trt_sum, soil:lamss) %>% 
  distinct() %>% 
  pivot_longer(soil:lamss) %>% 
    group_by(till_id, year, straw_id, cctrt_id, trt_sum, name) %>% 
    summarise(value = sum(value))

# sum across years
e1 %>% 
  group_by(till_id, year, straw_id, cctrt_id, trt_sum, name) %>% 
  summarise(value = sum(value)) %>% 
  ggplot() +
  geom_tile(aes(x = straw_id, y = name, fill = value/100)) +
  scale_fill_viridis_c() +
  facet_grid(till_id~cctrt_id)

#--keep year separate
e1 %>% 
  ggplot() +
  geom_tile(aes(x = straw_id, y = name, fill = value/100)) +
  scale_fill_viridis_c(direction = -1, option = "rocket") +
  facet_grid(till_id~cctrt_id+year)

#--I bet the 'success' of the cover crop was inversely related to diversity?



# impacts on diversity ----------------------------------------------------


l3 <- lmer(shan_hill ~ cc_trt + (1|loc_sys), data = cc_div_tests)
lg2 <- glmer(shan_hill ~ cc_trt + (1|loc_sys), family = poisson, data = cc_div_tests)
summary(l3)
ggResidpanel::resid_panel(l2)
cc_div_tests$fit_div <- predict(l2)

hist(cc_div_tests$shan)
visreg(l3, gg = TRUE, 
       line = list(col = mycols[1], lwd = 3),
       points = list(size = 3, pch = 21, fill = "black")) + theme_bw() +
  labs(x = "Cover Crop Treatmnet",
       y = "Shannon Hill Diversity")+
  theme_bw() +
  theme(axis.title        = element_text(size = 16),
        axis.text         = element_text(size = 14))
