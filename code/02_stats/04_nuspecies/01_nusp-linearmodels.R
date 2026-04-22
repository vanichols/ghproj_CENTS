#--do stats on number of species in communities
#--created 9 oct 2025
#--note that it is count data...
#--started going through 16 april

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


# models ------------------------------------------------------------------

m1 <- glmmTMB(nu_sp ~ till_id * cctrt_id * straw_id * yearF +
          (1|block_id/straw_id/till_id/cctrt_id),
        family=poisson(link = "log"), 
        data = d)

#--doesn't converge
#--try using glmer from lme4
m1a <- glmer(
  nu_sp ~ till_id * cctrt_id * straw_id * yearF +
    (1 | block_id / straw_id / till_id / cctrt_id),
  data = d,
  family = poisson(link = "log")
)

#--singular fit
Anova(m1a)

#--simplify the random term
m2 <- glmmTMB(nu_sp ~ till_id * cctrt_id * straw_id * yearF +
                (1|block_id/straw_id/till_id),
              family=poisson(link = "log"), 
              data = d)


Anova(m2)

#--these don't look good
sim_resp2 <- simulateResiduals(m2)
plot(sim_resp2)

#--oof this is just a very small piece, does it matter
Anova(m2)


m0 <- m2

em1 <- emmeans(m2, ~cctrt_id)
em1a <- emmeans(m2, ~cctrt_id, type = "response")
em1a |> 
  as_tibble() |> 
  write_xlsx("data/stats/supp_tables/spnu-cc-estimates.xlsx")

pairs(em1a) |> 
  as_tibble() |> 
  arrange(-ratio)

est_list = list('radishes' = c(0, 0, 1, 1, 0),
                'mixE' = c(1, 0, 0, 0, 0))

# Now we call "contrasts", after back-transforming using regrid(), and feeding
# in our list of estimates, while turning off the actual testing against zero
# (we don't care about that p-value) and turn of the generation of confidence
# intervals:
pairs(contrast(regrid(em1), est_list, infer = c(T, F)))

em1a

emmeans(m2, ~yearF, type = "response") |> 
  as_tibble() |> 
  write_xlsx("data/stats/supp_tables/spnu-year-estimates.xlsx")
