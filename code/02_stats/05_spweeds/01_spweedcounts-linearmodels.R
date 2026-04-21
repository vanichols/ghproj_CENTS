# created 27 jan 2026
# purpose: do stats on spring weed counts
# notes: 3 subreps
#        I averaged over subreps bc the model is complicated already
#        weed_type has four categories (dicots (A), monocots (A), cirar (P) and equar (P))
#        weed_type2 has two categories (A, P)
#   Q1: impact on total number of weeds
#   Q2: impact on perennial weeds

#--Simon Qs: How to account for the repeated measures?
#          (they returned to the same plots in year 1 and in year 2)
#            Full model that includes weed_type2 doesn't converge
#            Full model on only perennials doesn't converge (zeros?)

library(tidyverse)
library(CENTSdata)
library(writexl)

library(glmmTMB)
# library(lme4)
# library(lmerTest)
library(broom)
library(emmeans)
library(performance)
library(DHARMa)
library(car)
library(tweedie)

rm(list = ls())

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_spweedcount)

#--assign weed_type2 (A or P)
draw <- 
  y %>% 
  mutate(year = year(date2),
         yearF = paste0("Y", year)) %>% 
  left_join(eu) %>% 
  mutate(weed_type2 = ifelse(weed_type %in% c("dicot", "monocot"), "A", "P"))


# viz ---------------------------------------------------------------------

#--there are lots of zeros, but this doesn't mean it is zero inflated
draw %>% 
  ggplot(aes(count)) +
  geom_histogram()

#--lots of 0s in the perennial category
draw %>% 
  ggplot(aes(count)) +
  geom_histogram(aes(fill = weed_type2))

#--more weeds in 2019
#--inversion has highest
draw %>% 
  ggplot(aes(cctrt_id, count)) +
  geom_boxplot(aes(color = weed_type2)) +
  facet_grid(year~till_id) 


draw %>% 
  summarise(ave = mean(count),
            var = sd(count)^2)

draw |> 
  ggplot(aes(subplot_id, count)) +
  geom_col(aes(fill = weed_type)) +
  facet_grid(yearF~subrep)

#--average total count within a subrep
draw |> 
  group_by(yearF, subplot_id, subrep) |> 
  summarise(totcount = sum(count)) |> 
  ggplot(aes(yearF, totcount)) +
  geom_beeswarm()

#--overall mean of 128
draw |> 
  group_by(yearF, subplot_id, subrep) |> 
  summarise(totcount = sum(count)) |>
  mutate(dummy = "A") |> 
  ggplot(aes(dummy, totcount)) +
  geom_beeswarm()

draw |> 
  group_by(yearF, subplot_id, subrep) |> 
  summarise(totcount = sum(count)) |> 
  ungroup() |> 
  summarise(meancount = mean(totcount))


# 0. make forms of data ---------------------------------------------------

#--first, take the average across subreps, keeping all the descriptors
d <- 
  draw %>% 
  group_by(yearF, 
           weed_type, #--cirar, equar, dicot, monooct 
           weed_type2, #-P, A
           block_id, plot_id, 
           straw_id, till_id, cctrt_id) %>% 
  summarise(count = mean(count)) %>% 
  ungroup()

#--now sum to get the total A and P
d_type2tot <- 
  d |> 
  group_by(yearF, 
           #weed_type, #--cirar, equar, dicot, monooct 
           weed_type2, #-P, A
           block_id, plot_id, 
           straw_id, till_id, cctrt_id) %>%
  summarise(count2 = sum(count))


hist(d_type2tot$count2)

#--if you have any fixed effect trts w/count data and it is all 0s, you have a problem
#--any trt combos w-all 0s?
#--yes. this is a problem. 
d_type2tot |> 
  #filter(weed_type2 == "A") |> 
  group_by(yearF, weed_type2, straw_id, till_id, cctrt_id) |> 
  summarise(mn = mean(count2)) |> 
  arrange(mn) 

#--now sum to get the total weed count
d_tot <- 
  d |> 
  group_by(yearF, 
           #weed_type, #--cirar, equar, dicot, monooct 
           #weed_type2, #-P, A
           block_id, plot_id, 
           straw_id, till_id, cctrt_id) %>%
  summarise(count_tot = sum(count))



# 0. considerations -------------------------------------------------------

#--poisson requires counts (integers), we don't have that
#--binomial is for proportions (ex 2 of the 5 quadrats), so not appropriate here
#--gamma is...I think like a poisson but doesn't require integers

#--over-dispersion is considered using a poisson, only bc has only one parameter
#--negative binomial is poisson plus a dispersion parameter (requires counts)
#--tweedie doesn't require counts
#--summary from the internet
#Gamma is for positive continuous data,
#Negative Binomial handles over-dispersed count data, 
#Tweedie (a mixture of Poisson and Gamma) models, 
# continuous data with exact zeros

#--so def don't use binomial, fit models to decide about over dispersion

# In the tweedie distribution, power parameter should be between 1 and 2 (at
# either of those extremes, (the tweedie reduces to either gamma or poisson
# distributions). Play around with mean, dispersion (= phi) and power values to
# get a feel for the distributions
hist(rtweedie(n = 1e4, mu = 1, phi = 1, power = 1))
hist(rtweedie(n = 1e4, mu = 1, phi = 1, power = 1.5))
hist(rtweedie(n = 1e4, mu = 1, phi = 1, power = 2))


# 1. model on total weeds-------------------------------------------------------------------

#--data we are using, no 0s?
d_tot |> 
  group_by(yearF, straw_id, till_id, cctrt_id) |> 
  summarise(mn = mean(count_tot)) |> 
  arrange(mn)

hist(d_tot$count_tot)

#--full model, doesn't account for repeated measures?
m2_t1 <- glmmTMB(count_tot ~ yearF * till_id * straw_id * cctrt_id
                + (1 | block_id/straw_id/till_id/cctrt_id),
                family = tweedie(),
                data = d_tot)

sim_rest1 <- simulateResiduals(m2_t1)
plot(sim_rest1)
sim_rest2 <- resid(sim_rest1, quantileFunction = qnorm, outlierValues = c(-5, 5))
boxplot(sim_rest2 ~ d_tot$straw_id)
boxplot(sim_rest2 ~ d_tot$till_id)
ggResidpanel::resid_auxpanel(sim_rest2, predict(m2_t1))

#--to see what the random effects is contributing
#--note it is a log scale, but looking at them relatively can be informative
VarCorr(m2_t1)

#--look at the dispersion parameter for the tweedie
#--it is not zero and it is not huge
#--so there is nothing catastrophically wrong
sigma(m2_t1)


#--just as an example if heterosc is suspected
m2_t2 <- glmmTMB(count_tot ~ yearF * till_id * straw_id * cctrt_id 
                + (1 | block_id/straw_id/till_id/cctrt_id),
                dispformula = ~till_id,
                family = tweedie(),
                data = d_tot)

#--BIC is more conservative
#--the fact that the model you chose depends on the criteria means its a wash
AIC(m2_t1, m2_t2)
compare_performance(m2_t1, m2_t2, metrics = c('AIC', 'BIC', 'AICc'))

m0 <- m2_t2


joint_tests(m0) |> 
  as_tibble() |> 
  mutate(p.val_highlight = ifelse(p.value < 0.05, "SIG", " ")) |> 
  write_xlsx("data/stats/supp_tables/spweedcounts-anova.xlsx")


# total weeds inference ---------------------------------------------------------------

#--year x straw
#--impact in 2019, but not in 2020
emmeans(m0, specs = ~ straw_id|yearF, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(reorder(straw_id, response), response, color = straw_id)) +
  geom_point() +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  facet_grid(.~yearF)

pairs(emmeans(m0, specs = ~ straw_id|yearF, type = "response")) |> 
  as_tibble() |> 
  write_xlsx("data/stats/supp_tables/spnu-strawyear-pairs.xlsx")
  


emmeans(m0, specs = ~ till_id|cctrt_id|yearF, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(reorder(cctrt_id, response), response, color = cctrt_id)) +
  geom_point() +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  facet_grid(till_id~yearF)

#--till x cctrt x year
emmeans(m0, specs = ~ till_id|cctrt_id|yearF, type = "response") |> 
  as_tibble() |> 
  write_xlsx("data/stats/supp_tables/spnu-cctillyear-estimates.xlsx")
  

pairs(emmeans(m0, specs = ~ cctrt_id|till_id|yearF, type = "response")) |> 
  as_tibble() |> 
  filter(till_id == "notill") |> 
  write_xlsx("data/stats/supp_tables/spnu-cctillyear-pairs.xlsx")

pairs(emmeans(m0, specs = ~ cctrt_id|till_id|yearF, type = "response")) |> 
  as_tibble() |> 
  filter(till_id == "surface")

#--year impacts
(emmeans(m0, specs = ~ yearF, type = "response"))
  
pairs(emmeans(m0, specs = ~ yearF)) |> 
  as_tibble()

#--tillage
(emmeans(m0, specs = ~ till_id, type = "response"))

pairs(emmeans(m0, specs = ~ yearF)) |> 
  as_tibble()

#--straw

emmeans(m0, specs = ~ straw_id|yearF, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(reorder(straw_id, response), response, color = straw_id)) +
  geom_point() +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  facet_grid(.~yearF)


# 2. model that includes weed type2-------------------------------------------------------------------

d_type2tot |> 
  ggplot(aes(weed_type2, count2)) +
  geom_col()


#--full model, doesn't converge
m1_t1 <- glmmTMB(count2 ~ yearF * till_id * straw_id * cctrt_id * weed_type2
                 + (1 | block_id/straw_id/till_id/cctrt_id),
                 family = tweedie(),
                 data = d_type2tot)

#--singluar convergence means one thing is estimated at zero
#--if that is the only thing that pops up, it isn't such a problem 
#--but if there is another issue, then don't ignore it
#--so you can start by looking at what MIGHT be zero
VarCorr(m1_t1)

#--so straw 'effect' is very small
#--to keep the information about tillage being nested within straw, use :

m1_t2 <- glmmTMB(count2 ~ yearF * till_id * straw_id * cctrt_id * weed_type2
                 + (1 | block_id/straw_id:till_id/cctrt_id),
                 family = tweedie(),
                 data = d_type2tot)

#--didn't fix it
#--try saying the correlation within a year is a stronger pattern
#--the alternative is that the annual is correlated over time

m1_t3 <- glmmTMB(count2 ~ yearF * till_id * straw_id * cctrt_id * weed_type2
                 + (1 | block_id/straw_id:till_id/cctrt_id/yearF) ,
                 family = tweedie(),
                 data = d_type2tot)

m1_t4 <- glmmTMB(count2 ~ yearF * till_id * straw_id * cctrt_id * weed_type2
                 + (1 | block_id/straw_id:till_id/cctrt_id/weed_type2) ,
                 family = tweedie(),
                 data = d_type2tot)


#--we are going to try a transformation!
#--this is probably the best we have
m1_t5 <- glmmTMB(log(count2+1) ~ yearF * till_id * straw_id * cctrt_id * weed_type2
                 + (1 | block_id/straw_id/till_id/cctrt_id/weed_type2) ,
                 family = gaussian(),
                 REML = T,
                 #dispformula = ~weed_type2:till_id,
                 data = d_type2tot)

d_type2tot |> 
  filter(till_id == "surface", weed_type2 == "P")

sim_rest1 <- simulateResiduals(m1_t5)
plot(sim_rest1)

#--we know it looks like crap, but as long as we undersatnd that, its ok
#--is the dataset balanced??
#--this might be the best we can do
#--we are violating heteroscadascity 

m0p <- m1_t5

# perennial weed numbers for starburst ---------------------------------------------------------------

emm_perennials <- emmeans(m0p, ~weed_type2:till_id:cctrt_id:straw_id, type = 'response', bias.adjust= T)

starburst <- 
  emm_perennials |> 
  as_tibble() |> 
  filter(weed_type2 == "P")

#--is it the same? Yes.
tidy(emm_perennials) |> 
  filter(weed_type2 == "P")

starburst |> 
  write_xlsx("data/stats/figs_emmeans/starburst-emmeans-pweeds.csv")


# weeds for correlation ---------------------------------------------------

emm_all <- emmeans(m0p, ~weed_type2:till_id:cctrt_id:straw_id:yearF, type = 'response', bias.adjust= T)

emm_all |> 
  as_tibble() |> 
  write_csv("data/stats/figs_emmeans/emmeans-sfig-spweedcounts.csv")

# perennial weeds inference -----------------------------------------------

joint_tests(m0p) |> 
  mutate(p.value_highlight = ifelse(p.value < 0.05, "SIG", " ")) |> 
  write_xlsx("data/stats/supp_tables/spnuPERENN-anova.xlsx")

#--year straw weedtype
emmeans(m0p, ~straw_id|yearF:weed_type2, 
        type = "response", 
        bias.adjust= T) |> 
  as_tibble() |> 
  filter(weed_type2 == "P") |> 
  ggplot(aes(reorder(straw_id, response), response, color = straw_id)) +
  geom_point() +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL)) +
  facet_grid(.~yearF)
  
pairs(emmeans(m0p, ~straw_id|yearF:weed_type2, type = "response", bias.adjust= T)) |> 
  as_tibble() |> 
  filter(weed_type2 == "P") 
  
#--compare tillages
pairs(emmeans(m0p, ~till_id|weed_type2, type = 'response', bias.adjust= T)) |> 
  as_tibble() |> 
  filter(weed_type2 == "P") |> 
  mutate(p.val_highlight = ifelse(p.value < 0.05, "SIG", " ")) 

emmeans(m0p, ~till_id|weed_type2, type = 'response', bias.adjust= T) |> 
  as_tibble() |> 
  filter(weed_type2 == "P") |> 
  write_xlsx("data/stats/supp_tables/spnuPERENN-till-estimates.xlsx")

#--compare tillages in each cctrt/straw
pairs(emmeans(m0p, ~till_id|weed_type2:cctrt_id:straw_id, type = 'response', bias.adjust= T)) |> 
  as_tibble() |> 
  filter(weed_type2 == "P") |> 
  mutate(p.val_highlight = ifelse(p.value < 0.05, "SIG", " ")) |> 
  arrange(contrast, cctrt_id, straw_id)

#--
pairs(emmeans(m0p, ~till_id|weed_type2:cctrt_id:straw_id, type = 'response', bias.adjust= T)) |> 
  as_tibble() |> 
  filter(weed_type2 == "P") |> 
  write_xlsx("data/stats/supp_tables/spnuPERENN-till-pairs.xlsx")

pairs(emmeans(m0p, ~cctrt_id|weed_type2:till_id:straw_id, type = 'response', bias.adjust= T)) |> 
  as_tibble() |> 
  filter(weed_type2 == "P")  |> 
  mutate(p.val_highlight = ifelse(p.value < 0.05, "SIG", " ")) |> 
  write_xlsx("data/stats/supp_tables/spnuPERENN-cc-pairs.xlsx")


#--averaged over NT and surface treatments
#--compare MixE to NoCC
em_test1 <- (emmeans(m0p, ~cctrt_id|till_id|weed_type2, type = 'response', bias.adjust= T))

pairs(emmeans(m0p, ~cctrt_id|till_id|weed_type2, type = "response"))

complist1 <- list('mixE vs Nocc' = c(1, 0, -1, 0, 0))

tidy(contrast(regrid(em_test1, type = 'response', bias.adjust = T), complist1))
  
# fig values -----------------------------------------------------------------


#--don't want year included
#--get estimates of total weeds and error bars with them
emm_tot = emmeans(m0p, ~weed_type2|till_id:cctrt_id, type = 'response', bias.adjust= T)
est_list = list('Total' = c(1, 1))
res_tot <- 
  tidy(contrast(regrid(emm_tot, type = 'response', bias.adjust = T), est_list, infer = c(T, T))) |> 
  rename(weed_type2 = contrast)

#--get estimates of perennial weeds and error bars with them
emm_sep <- emmeans(m0p, ~weed_type2|till_id:cctrt_id, type = 'response', bias.adjust= T)
res_sep <- 
  tidy(emm_sep) |> 
  rename(estimate = response, null.value = null)

res <- 
  res_tot |> 
  bind_rows(res_sep)

res |> 
  write_csv("data/stats/figs_emmeans/emmeans-fig4-spweedcounts.csv")


# #--let's do emmeans together! (old from simon)
# 
# #--get estimates of total weeds and error bars with them
# emm_tot = emmeans(m0p, ~weed_type2|till_id:cctrt_id:yearF, type = 'response', bias.adjust= T)
# est_list = list('Total' = c(1, 1))
# res_tot <- 
#   tidy(contrast(regrid(emm_tot, type = 'response', bias.adjust = T), est_list, infer = c(T, T))) |> 
#   rename(weed_type2 = contrast)
# 
# #--get estimates of perennial weeds and error bars with them
# emm_sep <- emmeans(m0, ~weed_type2|till_id:cctrt_id:yearF, type = 'response', bias.adjust= T)
# res_sep <- 
#   tidy(emm_sep) |> 
#   rename(estimate = response, null.value = null)
# 
# res <- 
#   res_tot |> 
#   bind_rows(res_sep)
# 
# res |> 
#   write_csv("data/stats/figs_emmeans/emmeans-fig4-spweedcounts.csv")
# 
