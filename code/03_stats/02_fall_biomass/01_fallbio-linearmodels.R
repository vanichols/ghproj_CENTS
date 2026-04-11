# created 5 nov 2024
# purpose: do stats on fall biomass
# notes: It is not clear if in 2019 the volunteers were omitted, 
# -- or if they got lumped in another cat
# -- Bo says they got lumped in 'non crop' category
# -- must combine volunteer and weeds data into 'non-cover crop'
#--23 april 2024 update, don't run separate models for each year except to get letters

#--simon check

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
  summarise(dm_gm2 = sum(dm_gm2, na.rm = T))

dtot |> 
  ggplot(aes(cctrt_id, dm_gm2)) +
  geom_point() +
  facet_grid(.~weayear)

# tot biomass models ------------------------------------------------------------------

m1 <- glmmTMB(dm_gm2 ~ till_id * cctrt_id * straw_id * weayear + 
                (1 | block_id/straw_id/till_id/cctrt_id), 
              data = dtot)

#--model convergence problem

#--try tweedie?
m1 <- glmmTMB(dm_gm2 ~ till_id * cctrt_id * straw_id * weayear + 
                (1 | block_id/straw_id/till_id/cctrt_id), 
              family = tweedie(link = "log"),
              data = dtot)


sim_rest1 <- simulateResiduals(m1)
plot(sim_rest1)

#--this allows us to see if variance should differ by a group
sim_rest2 <- resid(sim_rest1, quantileFunction = qnorm, outlierValues = c(-5, 5))

boxplot(sim_rest2 ~ dtot$straw_id)
boxplot(sim_rest2 ~ dtot$till_id)
boxplot(sim_rest2 ~ dtot$cctrt_id)
boxplot(sim_rest2 ~ dtot$weayear)

#--works with lme...why?
m3 <- lme(dm_gm2 ~ till_id * straw_id * cctrt_id * weayear,
          random = ~1 | block_id/straw_id/till_id/cctrt_id,
          data = dtot)

#--ugh lme is weird
raw_resid <- residuals(m3, type = "response")   # raw residuals
pearson_resid <- residuals(m3, type = "pearson") # standardized residuals

plot(fitted(m3), pearson_resid,
     xlab = "Fitted values", ylab = "Pearson residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(resid(m3))
qqline(resid(m3))


VarCorr(m3)

m2 <- lme(dm_gm2 ~ till_id * straw_id * cctrt_id * weayear,
          random = ~1 | block_id/straw_id/till_id/cctrt_id,
          data = dtot,
          weights = varIdent(form = ~1 | weayear))

raw_resid <- residuals(m2, type = "response")   # raw residuals
pearson_resid <- residuals(m2, type = "pearson") # standardized residuals

plot(fitted(m2), pearson_resid,
     xlab = "Fitted values", ylab = "Pearson residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(resid(m2))
qqline(resid(m2))


VarCorr(m2)


#--the glmtmb I guess
compare_performance(m1, m2, m3, metrics = c('AIC', 'BIC', 'AICc'))

#--check with Simon, then proceed
m0 <- m1

#--use Anova for glmmTMB, anova for lme
Anova(m0) |>
  rownames_to_column() |>
  as_tibble() |>
  write_xlsx("data/stats/supp_tables/fallbio-anova.xlsx")


# letters -----------------------------------------------------------------

em1 <- emmeans(m0, specs = ~cctrt_id|till_id|weayear)

cld1 <- 
  multcomp::cld(em1, by = 'weayear', Letters = letters) |> 
  as_tibble() |> 
  mutate(.group = ifelse(weayear == "Y2019", str_to_upper(.group), .group))

cld1 |> 
  write_csv("data/stats/figs_emmeans/emmeans-fig2-fallbio.csv")


# starburst chart --------------------------------------------------

#--averaged over years
emmeans(m0, specs = ~ cctrt_id|till_id|straw_id, type = "response") |> 
  as_tibble() |> 
  mutate(perf_cat = "fallbio") |> 
  write_csv("data/stats/figs_emmeans/starburst-emmeans-fallbio.csv")

# interpret chosen model --------------------------------------------------

Anova(m0) |>
  rownames_to_column() |> 
  as_tibble() |>
  janitor::clean_names() |> 
  filter(pr_chisq < 0.05) |> 
  mutate(pvalue2 = round(pr_chisq, 4))

#--tillage and cctrt interact with year

#--the tillage interaction is an amplification
emmeans(m0, specs = ~ till_id|weayear, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(reorder(till_id, emmean), emmean, color = till_id)) +
  geom_point() +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL)) +
  facet_grid(.~weayear)

pairs(emmeans(m0, specs = ~ till_id))

#--the cctrt interaction is the mixE changing from year to year
emmeans(m0, specs = ~ cctrt_id|weayear, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(reorder(cctrt_id, emmean), emmean, color = cctrt_id)) +
  geom_point() +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL)) +
  facet_grid(.~weayear)

#--tillage and cctrt interact with each other
#--it is basically an amplification, reducing tillage amplifes cctrt effect
emmeans(m0, specs = ~ till_id|cctrt_id, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(reorder(cctrt_id, emmean), emmean, color = cctrt_id)) +
  geom_point() +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL)) +
  facet_grid(.~till_id)

#-----stopped