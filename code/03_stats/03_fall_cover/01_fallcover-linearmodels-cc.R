# created 5 nov 2024
# purpose: do stats on vegetation cover
# notes: 3 subreps make up a single eu

# 4 cats of cover: soil cover, weed cover, covercrop, volunteer

# results: model is not fitting well...
# issues: there is one treatment combination that had 0s in all instances
#         soil cover
# march 2026 simon fixed it
# 9 april 2026, started cleaning up for publishing
#-- simon - huge error bar?


library(CENTSdata)

library(tidyverse)
library(writexl)

library(broom)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(car)

rm(list = ls())

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_fallpctcover)
w <- read_csv("data/tidy_weaclass.csv")

#--data separated by species
d_sp <- 
  eu %>% 
  left_join(y) %>% 
  mutate(year = year(date2)) %>% 
  left_join(w) %>% 
  mutate(weayear = paste(precip, te, sep  = "-"),
         cover_frac = cover_pct / 100)

#--data separated by cateogry (covercrop, soil, other)
d_cat <-
  d_sp %>% 
  group_by(block_id, plot_id, subplot_id, till_id, straw_id, 
           cctrt_id, subrep, weayear, cover_cat) %>% 
  summarise(cover_pct = sum(cover_pct),
            cover_frac = sum(cover_frac)) 

#--simon says we can model each category separately
#--Thoughts on which family to use
#--normally use a beta for continuous proportions, but 
#--here, bc there may be some values of 0, we use ordbeta
#--the logit link maps the linear predictor to the data scale (which is 0-1)
#--there may be subtle differences between choices, logit and probit are the most common

# model on covercrop cover category-------------------------------------------------------------------

#--run model on cover crop fraction
#--there is one combination that has a zero and it makes the error bar weird
#--is that something to be concerned about?
m1 <- glmmTMB(cover_frac ~ till_id * cctrt_id * straw_id * weayear +
                   (1|block_id/straw_id/till_id/cctrt_id),
                 family=ordbeta(link = "logit"), 
              data=d_cat |> filter(cover_cat == "covercrop"))

#--lots of interactions
Anova(m1)
m1_simres <- simulateResiduals(m1)
plot(m1_simres)

#--the model fits well, do I need to account for more 0s in the nocc treatment? No.
#--the ordbeta is 'already' accounting for the zeros
#--you COULD model it separately for each category
#--it doesn't seem to be necessary bsaed on the plots
#--you WOULD use this ziformula to do that

#--account for difference in zeros by cover crop trt
m1zinfc <- update(m1, ziformula = ~cctrt_id)
m1z_simres <- simulateResiduals(m1zinfc)
plot(m1z_simres)

#--you can compare the models
#--ignore the pvalues here - it is only valid for nested models!!
anova(m1, m1zinfc)
#--the extra complication is not worth it (AIC and BIC values are lower for simpler model)

#--write final model anova table
tidy(car::Anova(m1)) |> 
  write_xlsx("data/stats/supp_tables/fallcover-cc-anova.xlsx")
  


# emmeans cover crop-----------------------------------------------------------------

#--so we have all these super complicated interactions
#--is there a way to simplify it, and justify only looking at the most impactful ones?

#--the nocc has a 0, so this huge error bar is to be expected
#--patterns are the same across years except for MixE
emmeans(m1, specs = ~ till_id|cctrt_id|weayear, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(reorder(cctrt_id, response), response, color = cctrt_id)) +
  geom_point() +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  facet_grid(till_id~weayear)

#--cc coverage from a trt depends on the weayear
#--MixE and MixM were extremely variable
#--nocc was solidly low (should be 0), 
#--radL was solid (low though)
#--radM was solid and high

#--tillage? cctrt ranking stayed the same
emmeans(m1, specs = ~ cctrt_id|till_id, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(reorder(cctrt_id, response), response, color = cctrt_id)) +
  geom_point() +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  facet_grid(.~till_id)

#--tillage? ranking stayed the same across weather years
emmeans(m1, specs = ~ till_id|weayear, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(reorder(till_id, response), response, color = till_id)) +
  geom_point() +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  facet_grid(.~weayear)

#--cctrt comparisons within a weayear
#--kind of overwhelming
(emmeans(m1, specs = pairwise ~ cctrt_id:weayear))$contrasts |> 
  as_tibble() |> 
  separate(contrast, into = c("t1", "t2"), sep = " - ") %>% 
  separate(t1, into = c("cctrt1", "w1"), sep = " ") %>% 
  separate(t2, into = c("cctrt2", "w2"), sep = " ") |> 
  filter(w1 == w2) |> 
  arrange(w1, p.value) |> 
  mutate(psimp = round(p.value, 3))

#--within a cctrt
#--yup no change in nocc, radL or radM, write it
(emmeans(m1, specs = pairwise ~ cctrt_id:weayear))$contrasts |> 
  as_tibble() |> 
  separate(contrast, into = c("t1", "t2"), sep = " - ") %>% 
  separate(t1, into = c("cctrt1", "w1"), sep = " ") %>% 
  separate(t2, into = c("cctrt2", "w2"), sep = " ") |> 
  filter(cctrt1 == cctrt2) |> 
  arrange(cctrt1, p.value) |> 
  mutate(psimp = round(p.value, 3))

#--write it, this is the story
(emmeans(m1, specs = pairwise ~ cctrt_id:weayear))$contrasts |> 
  as_tibble() |> 
  separate(contrast, into = c("t1", "t2"), sep = " - ") %>% 
  separate(t1, into = c("cctrt1", "w1"), sep = " ") %>% 
  separate(t2, into = c("cctrt2", "w2"), sep = " ") |> 
  filter(cctrt1 == cctrt2) |> 
  arrange(cctrt1, p.value) |> 
  mutate(psimp = round(p.value, 3)) |> 
  write_xlsx("data/stats/emmeans/emmeans-pairs-fallcover-cc-cctrt-btwn-years.xlsx")

