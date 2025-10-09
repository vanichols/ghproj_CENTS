# created 17 feb 2025
# purpose: do stats on spring weed counts, proportion P
# notes: 3 subreps
#   11 may 2025, try different data structure to get log-odds?
#   Q3: the weed was x times more likely to be a perennial in X vs Y

library(tidyverse)
library(lme4)
library(lmerTest)
library(CENTSdata)
library(broom)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(car)

rm(list = ls())

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_spweedcount)
w <- read_csv("data/tidy_weaclass.csv")

draw <- 
  y %>% 
  mutate(year = year(date2),
         yearF = paste0("Y", year)) %>% 
  left_join(eu) %>% 
  left_join(w) %>% 
  mutate(weayear = paste(year, precip, sep = "-"),
         weed_type2 = ifelse(weed_type %in% c("dicot", "monocot"), "A", "P"))


# viz ---------------------------------------------------------------------

draw

#--there are lots of zeros!
draw %>% 
  ggplot(aes(count)) +
  geom_histogram()

draw %>% 
  ggplot(aes(count)) +
  geom_histogram(aes(fill = weed_type2))


#--need to sum up the sub-reps
d <- 
  draw %>% 
  group_by(year, yearF, weed_type, weed_type2, 
           block_id, plot_id, subplot_id, 
           straw_id, till_id, cctrt_id, 
           weayear) %>% 
  summarise(count = sum(count)) %>% 
  ungroup()

#--so, there is the proportion perennial weeds, 
#--and the number p weeds
d %>% 
  filter(weed_type2 == "P") %>% 
  ggplot(aes(till_id, count)) +
  geom_jitter() +
  facet_grid(.~cctrt_id)

#--want to get percentage of P and A weeds for this analysis
d2 <-
  d %>% 
  group_by(year, yearF, block_id, plot_id, subplot_id, straw_id, till_id, cctrt_id,
           weed_type2) %>% 
  summarize(cat_tot = sum(count)) %>% 
  group_by(year, yearF, block_id, plot_id, subplot_id, straw_id, till_id, cctrt_id) %>% 
  mutate(tot = sum(cat_tot),
         cat_pct = cat_tot/tot) 

glimpse(d2)

#retaining straw in inversion and surface tended to 
# increase weed counts, interesting
d2 %>%
  filter(weed_type2 == "P") %>% 
  group_by(year, yearF, straw_id, till_id, cctrt_id) %>% 
  summarise(tot = mean(tot),
            cat_pct = mean(cat_pct)) %>% 
  ggplot(aes(year, tot)) +
  geom_point(aes(size = cat_pct, colour = till_id)) +
  geom_line(aes(linetype = straw_id, color = till_id)) +
  facet_grid(till_id~cctrt_id)

d2 %>%
  filter(weed_type2 == "P") %>% 
  group_by(year, yearF, straw_id, till_id, cctrt_id) %>% 
  summarise(tot = mean(tot),
            cat_pct = mean(cat_pct)) %>% 
  ggplot(aes(year, tot)) +
  geom_point(aes(size = cat_pct, colour = cctrt_id)) +
  geom_line(aes(linetype = straw_id, color = cctrt_id)) +
  facet_grid(.~till_id)

d2 %>%
  filter(weed_type2 == "P") %>% 
  group_by(year, yearF, straw_id, till_id, cctrt_id) %>% 
  summarise(tot = mean(tot),
            cat_tot = mean(cat_tot),
            cat_pct = mean(cat_pct)) %>% 
  ggplot() +
  geom_point(aes(x = year, y = tot, size = tot), color = "black") +
  geom_point(aes(x = year, y = tot, size = cat_tot), color = "orange") +
  geom_line(aes(x = year, y = tot, linetype = straw_id)) +
  facet_grid(till_id~cctrt_id)


# 1. log odds -------------------------------------------------------------

#--model proportion of weeds that are perennial

d3 <-
  d2 %>% 
  filter(weed_type2 == "P") %>% 
  mutate(cat_prop = cat_pct / 100)

#--binomial error distribution
#--zero inflated
m1 <- glmmTMB(cat_pct ~ till_id + (1|block_id), 
               data = d3, 
              ziformula = ~ 1, 
               family = binomial)

#--the residuals look terrible
sim_res <- simulateResiduals(m1)
plot(sim_res)

#--I think this one is the winner?
m2 <- glmmTMB(cat_pct ~ till_id + (1|block_id), 
              data = d3, 
              ziformula = ~ 1, 
              family = beta_family())

#--the residuals look much better
sim_res2 <- simulateResiduals(m2)
plot(sim_res2)

#--but this is testing a bad model, even though AIC is lower
AIC(m1, m2)

Anova(m2)

#--nope doesn't work
m3 <- glmmTMB(cat_pct ~ cctrt_id*till_id, 
              data = d3, 
              ziformula = ~ 1, 
              family = beta_family())

#--this does
m4 <- glmmTMB(cat_pct ~ cctrt_id+till_id, 
              data = d3, 
              ziformula = ~ 1, 
              family = beta_family())
#--this does too
m5 <- glmmTMB(cat_pct ~ cctrt_id+till_id, 
              data = d3, 
              ziformula = ~ 1, 
              family = binomial)

#--m5 is better
AIC(m4, m5)

#--fit it with cctrt? Wish I could do the interaction,
#--why isn't it working with more than one term?
m3 <- glmmTMB(cat_pct ~ cctrt_id + (1|block_id), 
              data = d3, 
              ziformula = ~ 1, 
              family = beta_family())
Anova(m3)

sim_res3 <- simulateResiduals(m3)
plot(sim_res3)


d3 %>%
  mutate(grp = ifelse(cat_tot == 0, "zero", "other")) %>% 
  ggplot(aes(till_id, cat_pct)) +
  geom_jitter(width = 0.1) +
  facet_grid(.~grp)

#--yeah I guess there seems to be no cctrt pattern
d3 %>%
  mutate(grp = ifelse(cat_tot == 0, "zero", "other")) %>% 
  ggplot(aes(cctrt_id, cat_pct)) +
  geom_jitter(width = 0.1, aes(color = yearF)) +
  facet_grid(yearF~till_id) 

d3 %>%
  mutate(grp = ifelse(cat_tot == 0, "zero", "other")) %>% 
  ggplot(aes(cctrt_id, cat_pct)) +
  geom_jitter(width = 0.1) +
  facet_grid(.~till_id) 


#--how is there no diference btwn notill and inversion?
pairs(emmeans(m5, 
        ~ till_id, 
        type = "response"))
pairs(emmeans(m5, 
              ~ till_id))


#--how is there no diference btwn notill and inversion?
(emmeans(m5, 
              ~ till_id, 
              type = "response"))


emm_conditional <- emmeans(m5, 
                           ~ till_id, 
                           component = "cond",
                           type = "response")
summary(emm_conditional)
pairs(emm_conditional, reverse = TRUE)
1/.603

#--you will get two things, one for zeros and one for not zeros
emm_conditional3 <- emmeans(m3, 
                           ~ cctrt_id, 
                           component = "cond",
                           type = "response")
summary(emm_conditional3)
pairs(emm_conditional3, reverse = TRUE)
1/.603

# brms for zero hurdle model? ----------------------------------------------

library(brms)


# Zero hurdle beta model
model_hurdle <- brm(bf(cat_prop ~ yearF + till_id + (1|block_id), 
                       hu ~ yearF + till_id + (1|block_id)), 
                    family = hurdle_negbinomial(), 
                    data = d3)
summary(model_hurdle)

# -------------------------------------------------------------------------


