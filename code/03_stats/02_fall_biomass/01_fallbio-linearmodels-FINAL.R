# created 5 nov 2024
# purpose: do stats on fall biomass
# notes: It is not clear if in 2019 the volunteers were omitted, 
# -- or if they got lumped in another cat
# -- Bo says they got lumped in 'non crop' category
# -- must combine volunteer and weeds data into 'non-cover crop'


library(tidyverse)
library(CENTSdata)
library(writexl)

#library(lme4)
#library(lmerTest)
#library(broom)
library(nlme)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(car)
library(performance)

library(multcomp)

rm(list = ls())

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)

y <- 
  as_tibble(cents_fallbio) |> 
  mutate(weayear = paste0("Y", year(date2)))

#--data
d <- 
  eu %>% 
    left_join(y)

# viz ---------------------------------------------------------------------

#--there are a fair number of zeros in the data
d %>% 
  ggplot(aes(dm_gm2)) +
  geom_histogram()

d %>% 
  ggplot(aes(dm_gm2)) +
  geom_histogram() +
  facet_grid(.~dm_type)


# combine weeds and volunteers --------------------------------------------

d1 <- 
  d %>% 
  mutate(dm_type2 = case_when(
    dm_type == "volunteer" ~ "other",
    dm_type == "weeds" ~ "other",
    dm_type == "grass_cl" ~ "covercrop",
    dm_type == "radish" ~ "covercrop",
    TRUE ~ dm_type)
    ) %>% 
  group_by(subplot_id, date2, weayear, dm_type2) %>% 
  summarise(dm_gm2 = sum(dm_gm2, na.rm = T)) %>% 
  left_join(eu) 

#--they are not strongly related
d1 %>% 
  pivot_wider(names_from = dm_type2, values_from = dm_gm2)  %>% 
  left_join(eu) %>% 
  ggplot(aes(covercrop, other)) +
  geom_point(aes(color = cctrt_id))


# data for model on total biomass-------------------------------------------------------------------

dtot <- 
  d1 %>% 
  group_by(subplot_id,  
           eu_id, block_id, plot_id, till_id, 
           rot_id, straw_id, cctrt_id, weayear) %>% 
  summarise(Mg_ha = sum(dm_gm2, na.rm = T)*0.01,
            dm_gm2 = sum(dm_gm2, na.rm = T)) #--units conversion

dtot |> 
  ggplot(aes(cctrt_id, Mg_ha)) +
  geom_point() +
  facet_grid(.~weayear)

#--no zeros
dtot |> 
  ggplot(aes(Mg_ha)) +
  geom_histogram()

# tot biomass models ------------------------------------------------------------------

#--fails without REML = T specified...
#--works if units are changed to Mg_ha
#--if family is Gaussian (i.e. not specified), make REML = T
#--if you are comparing modesl with fixed effects, to pick your model turn REML off, then turn it back on once you pick

#--if Simon sees this 'iteration reached withint convergence', he just makes it work harder (inc number of iterations)

# m1 <- glmmTMB(dm_gm2 ~ till_id * cctrt_id * straw_id * weayear + 
#                 (1 | block_id/straw_id/till_id/cctrt_id), 
#               REML = T,
#               data = dtot)

#--just a gaussian distribution
#--it's not super strange that changing the changes in units helps it converge
m1 <- glmmTMB(Mg_ha ~ till_id * cctrt_id * straw_id * weayear + 
                (1 | block_id/straw_id/till_id/cctrt_id), 
              REML = T, 
              data = dtot)

summary(m1)
sim_rest1 <- simulateResiduals(m1)
#--doesn't look great
plot(sim_rest1)

sim_rest1 <- resid(sim_rest1, quantileFunction = qnorm, outlierValues = c(-5, 5))
boxplot(sim_rest1 ~ dtot$straw_id)
boxplot(sim_rest1 ~ dtot$till_id)
boxplot(sim_rest1 ~ dtot$cctrt_id)
boxplot(sim_rest1 ~ dtot$weayear)

#--could do the gamma, but first try the disp accounting for
#--tweedie doesn't make sense because I don't have 0s in total biomass

#--account for diff in weather years
m2 <- glmmTMB(Mg_ha ~ till_id * cctrt_id * straw_id * weayear + 
                (1 | block_id/straw_id/till_id/cctrt_id), 
              REML = T,
              dispformula = ~ weayear,
              data = dtot)
summary(m2)
#--doesn't work, run out of variance for the residuals, why? no idea

#--try using lme
m2 <- lme(Mg_ha ~ till_id * cctrt_id * straw_id * weayear, data = dtot,
           random = ~ 1 | block_id/straw_id/till_id/cctrt_id, 
         weights = varIdent(form = ~ 1| weayear))

#--works
summary(m2)
ggResidpanel::resid_panel(m2)


#--check with Simon, then proceed
m0 <- m2


joint_tests(m0)

#--use Anova for glmmTMB, anova for lme
#--Simon taught me joint_tests
joint_tests(m0) |>
  #rownames_to_column() |>
  as_tibble() |>
  write_xlsx("data/stats/supp_tables/fallbio-anova.xlsx")


# letters -----------------------------------------------------------------

#--these should give the same answer
em1a <- emmeans(m0, specs = ~cctrt_id|till_id|weayear, type = "response")
em1 <- emmeans(m0, specs = ~cctrt_id|till_id|weayear)

cld1 <- 
  multcomp::cld(em1a, by = 'weayear', Letters = letters) |> 
  as_tibble() |> 
  mutate(.group = ifelse(weayear == "Y2019", str_to_upper(.group), .group))

cld1 |> 
  write_csv("data/stats/figs_emmeans/emmeans-fig2-fallbio.csv")

cld1 |> 
  write_xlsx("data/stats/supp_tables/fallbio-letters.xlsx")

# starburst chart --------------------------------------------------

#--averaged over years
emmeans(m0, specs = ~ cctrt_id|till_id|straw_id, type = "response") |> 
  as_tibble() |> 
  mutate(perf_cat = "fallbio") |> 
  write_csv("data/stats/figs_emmeans/starburst-emmeans-fallbio.csv")


# interpret chosen model --------------------------------------------------

#--year main effect
emmeans(m0, specs = ~ weayear, type = "response") |> 
  as_tibble() |> 
  write_csv("data/stats/supp_tables/fallbio-year-estimates.xlsx")
  

#--the tillage interaction with year is an amplification
emmeans(m0, specs = ~ till_id|weayear, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(reorder(till_id, emmean), emmean, color = till_id)) +
  geom_point() +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL)) +
  facet_grid(.~weayear) +
  labs(title = "Fall biomass",
  subtitle = "Tillage interaction with year is an amplification effect")

pairs(emmeans(m0, specs = ~ till_id))

(emmeans(m0, specs = ~ till_id))

#--NT to surf
(1.96-1.58)/1.96

#--surf to inv
(1.58-1.35)/1.58

#--the cctrt interaction is the mixE changing from year to year
emmeans(m0, specs = ~ cctrt_id|weayear, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(reorder(cctrt_id, emmean), emmean, color = cctrt_id)) +
  geom_point() +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL)) +
  facet_grid(.~weayear) +
  labs(title = "Fall biomass",
       subtitle = "Cover crop interaction with year is a cross-over effect")

#--tillage and cctrt interact with each other
#--it is basically an amplification, reducing tillage amplifes cctrt effect
emmeans(m0, specs = ~ till_id|cctrt_id, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(reorder(cctrt_id, emmean), emmean, color = cctrt_id)) +
  geom_point() +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL)) +
  facet_grid(.~till_id) +
  labs(
    title = "Fall biomass",
    subtitle = "Tillage interaction with cover crop is an amplification")
