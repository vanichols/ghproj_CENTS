# created 28 aug 2025
# purpose: execute final stats models, get estimates needed for manu

library(tidyverse)
library(lme4)
library(lmerTest)
library(CENTSdata)
library(broom)
library(emmeans)
library(glmmTMB)
library(broom.mixed)
#--this one does letters
library(multcomp)

#--note: use dplyr::select bc of conflict issues

rm(list = ls())

#--helpful emmeans website: https://data-wise.github.io/doe/appendix/r-packages/emmeans.html

# 1. data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)



# 2. yields ---------------------------------------------------------------

#--data
d2a <- as_tibble(cents_cropyields)

d2 <- 
  eu %>% 
  left_join(d2a) %>% 
  mutate(estimate = yield_dry_Mgha)

#--checking data

d2 %>% 
  mutate(year = lubridate::year(date2)) %>% 
  ggplot(aes(crop, yield_dry_Mgha)) +
  geom_jitter(aes(shape = till_id, color = cctrt_id))

d2 %>% 
  group_by(crop) %>% 
  summarise(minY = min(yield_dry_Mgha),
            maxY = max(yield_dry_Mgha))

d2 %>% 
  ggplot(aes(crop, yield_dry_Mgha)) +
  geom_jitter()

#--model (chosen from exp-yields code)
m2 <- lmer(yield_dry_Mgha ~ cctrt_id * till_id * straw_id * cctrt_id * crop + 
              (1|block_id) +
              #(1|block_id:straw_id) +
              (1|block_id:straw_id:till_id),
            #(1|till_id:straw_id) +
            #(1|straw_id), 
            data = d2)

#--anova on full model
aov2 <- 
  as_tibble(anova(m2)) %>% 
  mutate(term = row.names(anova(m2))) %>% 
  dplyr::select(term, everything()) %>% 
  janitor::clean_names()

aov2 %>% 
  write_csv("data/stats_anova/anova_yield.csv")

#--impact of crop (duh), cctrt, no interaction
aov2 %>% 
  filter(pr_f < 0.05)
  

#--marginal means
em2_crop <- emmeans(m2, ~crop)
em2_cc <- emmeans(m2, ~cctrt_id)

#--radM is higher than all of them, all others equal
tidy(pairs(emmeans(m2, ~ cctrt_id))) %>% 
  filter(adj.p.value < .05)

#--letters (consistent with above)
l2_cc <- 
  tidy(multcomp::cld(em2_cc, Letters = LETTERS)) %>% 
  mutate(resp_var = "crop_yield")

l2_cc %>% write_csv("data/stats_letters/letters_yield_by-cc.csv")

#--contrasts
radM <- c(0, 0, 0, 0, 1)
other <- c(0.25, 0.25, 0.25, 0.25, 0)
contrast(em2_cc, method = list("radM - other" = radM - other))
tidy(em2_cc) %>% 
  filter(cctrt_id != "rad_M") %>% 
  summarise(estimate = mean(estimate))

.324/3.87 #--0.324 Mg is an increase of 8%




#--effect of crop, but no interactions
#--effect of cover crop, no interactions

# #--should present it as cover crop as that is the focus

# # cover crops -------------------------------------------------------------
# em_cc

