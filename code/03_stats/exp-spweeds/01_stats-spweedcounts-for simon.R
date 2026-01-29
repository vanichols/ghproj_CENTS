# created 27 jan 2026
# purpose: do stats on spring weed counts
# notes: 3 subreps
#   Q1: impact on total number of weeds
#   question for Simon: is this model appropriate? 
#     I averaged over subreps bc the model is complicated already

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

draw <- 
  y %>% 
  mutate(year = year(date2),
         yearF = paste0("Y", year)) %>% 
  left_join(eu) %>% 
  mutate(weayear = paste("Y", year),
         weed_type2 = ifelse(weed_type %in% c("dicot", "monocot"), "A", "P"))



# viz ---------------------------------------------------------------------

draw

#--there are lots of zeros!
draw %>% 
  ggplot(aes(count)) +
  geom_histogram()

#--lots of 0s in the perennial category
draw %>% 
  ggplot(aes(count)) +
  geom_histogram(aes(fill = weed_type2))


#--look at the data
#--more weeds in 2019
#--inversion has highest
draw %>% 
  group_by(weed_type2, cctrt_id, till_id, year) %>% 
  summarise(count = mean(count)) %>% 
  ggplot(aes(cctrt_id, count)) +
  geom_col(aes(fill = weed_type2)) +
  facet_grid(year~till_id) +
  labs(title = "Mean spring Weed Count") 


#--definitely over-dispersed (consider if using poisson, only bc has only one parameter)
#--bionomial is for proportions (ex 2 of 5 quadrats)
#--negative binomial is poisson plus a dispersion paramter
#--so def don't use binomial, fit models to decide about over dispersion
draw %>% 
  summarise(ave = mean(count),
            var = sd(count)^2)

#--need to deal with the sub-reps
d <- 
  draw %>% 
  group_by(year, yearF, weed_type, weed_type2, 
           block_id, plot_id, 
           straw_id, till_id, cctrt_id, 
           weayear) %>% 
  summarise(count = mean(count)) %>% 
  ungroup()

d %>% 
  group_by(weed_type2, cctrt_id, till_id, year) %>% 
  summarise(count = mean(count)) %>% 
  ggplot(aes(cctrt_id, count)) +
  geom_col(aes(fill = weed_type2)) +
  facet_grid(year~till_id) +
  labs(title = "Mean spring Weed Count") 

# 1. model on total weeds-------------------------------------------------------------------

#--sum the perennial and annuals tog
dtot <- 
  d %>%  
  group_by(year, yearF, 
           block_id, plot_id, 
           straw_id, till_id, cctrt_id) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() |> 
  mutate(count_int = round(count, 0))

dtot %>%
  select(count) |> 
  summary()

hist(dtot$count)

#//full model
m_t1 <- glmmTMB(count ~ yearF * till_id * straw_id * cctrt_id * weed_type
                + (1 | block_id/straw_id/till_id/cctrt_id),
                family = tweedie(),
                data = dtot)

#--this is the final one I chose
#--there are repeated measures
#--always start bvy fitting the full model

m_t1 <- glmmTMB(count ~ yearF * till_id * straw_id * cctrt_id 
                    + (1 | block_id/straw_id/till_id/cctrt_id),
                    family = tweedie(),
                    data = dtot)

sim_rest1 <- simulateResiduals(m_t1)
plot(sim_rest1)
sim_rest2 <- resid(sim_rest1, quantileFunction = qnorm, outlierValues = c(-5, 5))
boxplot(sim_rest2 ~ dtot$straw_id)
boxplot(sim_rest2 ~ dtot$till_id)
ggResidpanel::resid_auxpanel(sim_rest2, predict(m_t1))

#--to see what the random effects is contributing
#--note it is a log scale, but looking at them relatively can be informative
VarCorr(m_t1)

#--like a dispersion parameter for the tweedie, it is not zero and it is not huge
#--so there is nothing catastrophically wrong
sigma(m_t1)



#//just as an example if heterosc is suspected
m_t2 <- glmmTMB(count ~ yearF * till_id * straw_id * cctrt_id 
                + (1 | block_id/straw_id/till_id/cctrt_id),
                dispformula = ~till_id,
                family = tweedie(),
                data = dtot)

AIC(m_t1, m_t2)
library(performance)
compare_performance(m_t1, m_t2, metrics = c('AIC', 'BIC', 'AICc'))

