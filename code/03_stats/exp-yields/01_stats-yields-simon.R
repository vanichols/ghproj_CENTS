# created 15 oct 2024
# purpose: do stats on crop yields, using CENTSdata package
# notes: starting with basic response variable w/maarit's help
#--1 april 2025 - cleaned up and picked a model

library(tidyverse)
library(CENTSdata)

library(emmeans)
library(glmmTMB)
library(DHARMa)
library(performance)
library(broom.mixed)
library(lme4)
library(lmerTest)
library(nlme)

rm(list = ls())


# DK yields from DKstats---------------------------------------------------------------

dk_y <- 
  readxl::read_excel("data/dkstats-small-grain-yields-over-time.xlsx", 
                   skip = 2) %>% 
  select(6:ncol(.)) %>% 
  slice(1:6) %>% 
  janitor::clean_names() %>%
  mutate(x2009 = as.numeric(x2009)) %>% 
  pivot_longer(2:ncol(.), names_to = "year", values_to = "yield") %>% 
  mutate(year = str_remove(year, "x"),
         year = as.numeric(year)) 

dk_y %>% 
  group_by(crop) %>% 
  summarise(yield = mean(yield, na.rm = T)/10)

#--barley
dk_y %>% 
  filter((crop == "Spring barley" & year == 2018)) %>% 
  summarise(yield = mean(yield)/10)

#--oats
dk_y %>% 
  filter((crop == "Oats, mixed grains and other grains" & year == 2019)) %>% 
  summarise(yield = mean(yield)/10)

#--faba
dk_y %>% 
  filter((crop == "faba beans" & year == 2020)) %>% 
  summarise(yield = mean(yield)/10)

# CENTS data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_cropyields)

d <- 
  eu %>% 
  left_join(y) %>% 
  mutate(yield = yield_dry_Mgha)


# viz data ----------------------------------------------------------------

d$till_id %>% 
  unique()

d %>% 
  mutate(year = lubridate::year(date2)) %>% 
  ggplot(aes(crop, yield_dry_Mgha)) +
  geom_jitter(aes(shape = till_id, color = cctrt_id))

d %>% 
  group_by(crop) %>% 
  summarise(minY = min(yield_dry_Mgha),
            maxY = max(yield_dry_Mgha))

d %>% 
  ggplot(aes(crop, yield_dry_Mgha)) +
  geom_jitter()



# 0. considerations -------------------------------------------------------------------
# each crop represents one year
# run a separate model for each crop?


# 1. full model -----------------------------------------------------------------

d

m1 <- glmmTMB(yield ~ crop * till_id * cctrt_id * straw_id + 
                (1 | block_id/straw_id/till_id/cctrt_id), 
           data = d)

sim_rest1 <- simulateResiduals(m1)
plot(sim_rest1)

#--there seems to be some issues, might be bc of crop differences
sim_rest2 <- resid(sim_rest1, quantileFunction = qnorm, outlierValues = c(-5, 5))
boxplot(sim_rest2 ~ d$straw_id)
boxplot(sim_rest2 ~ d$till_id)
boxplot(sim_rest2 ~ d$crop)
#--seems like straw might need different variances, or crop
ggResidpanel::resid_auxpanel(sim_rest2, predict(m1))

#--to see what the random effects is contributing
#--note it is a log scale, but looking at them relatively can be informative
VarCorr(m1)

m2 <- glmmTMB(yield ~ crop * till_id * cctrt_id * straw_id + 
                (1 | block_id/straw_id/till_id/cctrt_id), 
              dispformula = ~straw_id,
              data = d)

sim2_rest1 <- simulateResiduals(m2)
plot(sim2_rest1)


#--is this the right way to do it, since crop is not included in the random effects?
m3 <- glmmTMB(yield ~ crop * till_id * cctrt_id * straw_id + 
                (1 | block_id/straw_id/till_id/cctrt_id), 
              dispformula = ~crop,
              data = d)


#--everyone prefers the third model
compare_performance(m1, m2, m3, metrics = c('AIC', 'BIC', 'AICc'))


#--I'm not sure how to work with glmmTMB outputs

#--variance differs by crop
m4 <- lme(yield ~ till_id * straw_id * cctrt_id * crop,
           random = ~1 | block_id/straw_id/till_id/cctrt_id,
           data = d,
           weights = varIdent(form = ~1 | crop))

#--they are the same
compare_performance(m3, m4, metrics = c('AIC', 'BIC', 'AICc'))

#--crop interactions are not important, actually
m5 <- lme(yield ~ till_id * straw_id * cctrt_id + crop,
          random = ~1 | block_id/straw_id/till_id/cctrt_id,
          data = d,
          weights = varIdent(form = ~1 | crop))

#--this tells us what we care about more...
anova(m5)

# interpret chosen model --------------------------------------------------

m4

#--rad_M is higher than all of the others
em1 <- emmeans(m5, ~cctrt_id)
pairs(em1)

#--rad_M is higher than all of the others - in every year?
em2 <- emmeans(m5, ~cctrt_id|crop)
pairs(em2)


res <- 
  emmeans(m4, ~cctrt_id|crop, type = "response") |> 
  as_tibble()


#--get values for figure (simon doesn't like box plots)
res |> 
write_csv("data/stats/emmeans/emmeans-yields-for-fig.csv")
