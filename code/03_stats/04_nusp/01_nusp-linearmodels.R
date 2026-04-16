#--do stats on number of species in communities
#--created 9 oct 2025
#--note that it is count data...

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

draw <- read_csv("data/tidy_nuspecies.csv")

d <- 
  draw %>% 
  left_join(cents_eukey) %>% 
  mutate(yearF = paste0("Y", year))


m_p1 <- glmmTMB(nu_sp ~ yearF*till_id * straw_id * cctrt_id 
                + (1 | block_id),
                #ziformula = ~1,
                family = poisson, 
                data = d)

m_p1

sim_resp1 <- simulateResiduals(m_p1)
plot(sim_resp1)

#--both negative binomials give similar answers
#--freaking everything is significant
Anova(m_p1)

emmeans(m_p1, ~cctrt_id, type = "response")
emmeans(m_p1, ~yearF, type = "response")

emmeans(m_p1, pairwise~cctrt_id, type = "response")

d %>% 
  ggplot(aes(nu_sp)) +
  geom_histogram(aes(fill = yearF), position = "dodge")
