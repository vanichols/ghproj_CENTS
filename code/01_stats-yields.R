# created 15 oct 2024
# purpose: do stats on crop yields, using new CENTSdata package
# notes: starting with basic response variable w/maarit's help

library(tidyverse)
library(lme4)
library(lmerTest)
library(CENTSdata)
library(broom)
library(emmeans)

rm(list = ls())

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_cropyields)

d <- eu %>% left_join(y)


# model -------------------------------------------------------------------

#note: run a separate model for each crop, each crop represents one year

# Q1 - trt impacts on yield -----------------------------------------------

d %>% 
  mutate(year = lubridate::year(date2)) %>% 
  ggplot(aes(crop, yield_dry_Mgha)) +
  geom_jitter(aes(shape = till_id, color = cctrt_id))

#--I don't think we need subplot_id, as yield was not repeatedly measured on a subplot
#--failed to converge
m1 <- lmer(yield_dry_Mgha ~ till_id * cctrt_id * straw_id * crop + 
             (1|block_id) +
           #  (1|subplot_id:cctrt_id:till_id:straw_id) +
             (1|cctrt_id:till_id:straw_id) + 
             (1|till_id:straw_id) +
             (1|straw_id), 
           data=d)

#--first look at the summary to trouble shoot
#--DenDF are 0...it is over-defined
summary(m1)

m2 <- lmer(yield_dry_Mgha ~ till_id * cctrt_id * straw_id * crop + 
             (1|block_id),
             #(1|cctrt_id:till_id:straw_id) + 
             #(1|till_id:straw_id) +
             #(1|straw_id), 
           data = d)

summary(m2)
anova(m2)
#--don't worry about what the DHARMa is flagging
library(DHARMa)
plot(simulateResiduals(m1))
plot(m2)
#--this looks good, the pattern is fine
qqnorm(resid(m2))
qqline(resid(m2))

anova(m2)

r <- tidy(anova(m2)) 

r %>% 
  write_csv("data/stats_yields.csv")

r %>% 
  filter(p.value < 0.05)

#--effect of crop, but no interactions
#--effect of cover crop
#--effect of tillage, depending on straw

emmeans(m2)
