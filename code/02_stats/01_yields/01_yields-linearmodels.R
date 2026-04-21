# created 15 oct 2024
# purpose: do stats on crop yields, using CENTSdata package
# notes: starting with basic response variable w/maarit's help
#--1 april 2025 - cleaned up and picked a model
#--9 april 2026 - rerunning and cleaning up

library(tidyverse)
library(CENTSdata)
library(writexl)


library(emmeans)
library(glmmTMB)
library(DHARMa)
library(performance)
library(nlme) #--has lme function


rm(list = ls())


# data --------------------------------------------------------------------

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



# 1. full model -----------------------------------------------------------------

d

m1 <- glmmTMB(yield ~ crop * till_id * cctrt_id * straw_id + 
                (1 | block_id/straw_id/till_id/cctrt_id), 
           data = d)

sim_rest1 <- simulateResiduals(m1)
plot(sim_rest1)

#--there seems to be some issues, might be bc of crop differences
#--this allows us to see if variance should differ by a group

sim_rest2 <- resid(sim_rest1, quantileFunction = qnorm, outlierValues = c(-5, 5))

boxplot(sim_rest2 ~ d$straw_id)
boxplot(sim_rest2 ~ d$till_id)
boxplot(sim_rest2 ~ d$crop)

#--seems like straw might need different variances, or crop
ggResidpanel::resid_auxpanel(sim_rest2, predict(m1))

#--to see what the random effects is contributing
#--note it is a log scale, but looking at them relatively can be informative
VarCorr(m1)

#--it might be straw

m2 <- glmmTMB(yield ~ crop * till_id * cctrt_id * straw_id + 
                (1 | block_id/straw_id/till_id/cctrt_id), 
              dispformula = ~straw_id,
              data = d)

sim2_rest1 <- simulateResiduals(m2)
plot(sim2_rest1)

#--try crop

m3 <- glmmTMB(yield ~ crop * till_id * cctrt_id * straw_id + 
                (1 | block_id/straw_id/till_id/cctrt_id), 
              dispformula = ~crop,
              data = d)


#--everyone prefers the third model
compare_performance(m1, m2, m3, metrics = c('AIC', 'BIC', 'AICc'))


#--outputs from glmmTBM are harder
#--here is the same model but in lme
#--variance differs by crop, in lme
m4 <- lme(yield ~ till_id * straw_id * cctrt_id * crop,
           random = ~1 | block_id/straw_id/till_id/cctrt_id,
           data = d,
           weights = varIdent(form = ~1 | crop))

#--they are the same
compare_performance(m3, m4, metrics = c('AIC', 'BIC', 'AICc'))

#--crop interactions are maybe not important?
#--because they are confounded with year, and are therefore expected
#--try eliminating them
m5 <- lme(yield ~ till_id * straw_id * cctrt_id + crop,
          random = ~1 | block_id/straw_id/till_id/cctrt_id,
          data = d,
          weights = varIdent(form = ~1 | crop))

#--much simpler model...
compare_performance(m4, m5, metrics = c('AIC', 'BIC', 'AICc'))

#--but if the yield does interact with the cover crop in certain crops...
anova(m5)

#--use the one stats says to use!
anova(m5) |> 
  rownames_to_column() |> 
  as_tibble() |> 
  write_xlsx("data/stats/supp_tables/yield-anova.xlsx")


# starburst chart --------------------------------------------------

#--averaged over year
emmeans(m5, specs = ~ cctrt_id|till_id|straw_id, type = "response") |> 
  as_tibble() |> 
  mutate(perf_cat = "yield") |> 
  write_csv("data/stats/figs_emmeans/starburst-emmeans-yield.csv")

# interpret chosen model --------------------------------------------------

#--rad_M is higher than all of the others?
em1 <- emmeans(m5, ~cctrt_id)
pairs(em1)
#--yes
#--write this table
pairs(em1) |> 
  as_tibble() |> 
  write_xlsx("data/stats/supp_tables/yield-cc-pairwise.xlsx")

em1 |> 
  as_tibble() |> 
  write_xlsx("data/stats/supp_tables/yield-cc-estimates.xlsx")


#--rad_M is higher than all of the others - in every year?
#--curious there isn't an interaction
#--the differenecs are most pronounced in faba bean
em2 <- emmeans(m4, ~cctrt_id|crop)
pairs(em2) |> 
  as_tibble() |> 
  separate(contrast, into = c("t1", "t2"), sep = " - ") %>%
  mutate(t1 = str_trim(t1),
         t2 = str_trim(t2)) |> 
  filter(t2 == "rad_M") |> 
  arrange(crop, p.value) |> 
  mutate(psimp = round(p.value, 2))

#--tillage...is not sig


#--get values for the figure, 
#--figure is separated by year because it needs to be
res <- 
  emmeans(m5, ~cctrt_id|crop, type = "response") |> 
  as_tibble()


#--get values for figure (don't plot raw values, plot modelled values)
res |> 
  write_csv("data/stats/figs_emmeans/emmeans-fig1-yields.csv")
