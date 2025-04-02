# created 15 oct 2024
# purpose: do stats on crop yields, using new CENTSdata package
# notes: starting with basic response variable w/maarit's help
#--1 april 2025 - I need to clean this up and pick a model and use it

library(tidyverse)
library(lme4)
library(lmerTest)
library(CENTSdata)
library(broom)
library(emmeans)
library(glmmTMB)
library(broom.mixed)

rm(list = ls())

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_cropyields)

d <- 
  eu %>% 
  left_join(y) %>% 
  mutate(estimate = yield_dry_Mgha)

d$till_id %>% 
  unique()

# model -------------------------------------------------------------------

#note: run a separate model for each crop, each crop represents one year

# Q1 - trt impacts on yield -----------------------------------------------

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


# 1. lmer -----------------------------------------------------------------

#--I don't think we need subplot_id, as yield was not repeatedly measured on a subplot
#--failed to converge
#--I think this notation might be backwards? and you only need one?
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

m1a <- lmer(yield_dry_Mgha ~ till_id * cctrt_id * straw_id * crop + 
             (1|block_id) +
             (1|cctrt_id:till_id:straw_id), #+ 
             #(1|till_id:straw_id) +
             #(1|straw_id), 
           data = d)

m1b <- lmer(yield_dry_Mgha ~ till_id * cctrt_id * straw_id * crop + 
              (1|block_id),# +
              #(1|cctrt_id:till_id:straw_id), #+ 
            #(1|till_id:straw_id) +
            #(1|straw_id), 
            data = d)

m1c <- lmer(yield_dry_Mgha ~ cctrt_id * till_id * straw_id * cctrt_id * crop + 
              (1|block_id) +
              #(1|block_id:straw_id) +
              (1|block_id:straw_id:till_id),
            #(1|till_id:straw_id) +
            #(1|straw_id), 
            data = d)



# 2. nlme -----------------------------------------------------------------

library(nlme)
# model <- lme(Y ~ W_Treat * S_Treat * SS_Treat, 
#              random = ~1 | Block/W_Treat/S_Treat, 
#              data = mydata)

m2d <- lme(yield_dry_Mgha ~ cctrt_id * till_id * straw_id * cctrt_id * crop,
           random = ~1 | block_id/straw_id/till_id,
            data = d)

#--variance differs by block
m2e <- lme(yield_dry_Mgha ~ cctrt_id * till_id * straw_id * cctrt_id * crop,
           random = ~1 | block_id/straw_id/till_id,
           data = d,
           weights = varIdent(form = ~1 | block_id))

#--variance differs by cctrt
m2f <- lme(yield_dry_Mgha ~ cctrt_id * till_id * straw_id * cctrt_id * crop,
           random = ~1 | block_id/straw_id/till_id,
           data = d,
           weights = varIdent(form = ~1 | cctrt_id))

#--compare hetergenous variances
anova(m2d, m2e)
anova(m2d, m2f)
anova(m2e, m2f)
AIC(m2d, m2e, m2f)

#m2e seems to be the best, but not statistically better than m2f
# accounting for some type of hetergeneous variance seems good
#--this gives a sig 3-way interaction
anova(m2e)
#--this gives a cctrt x crop interaction
anova(m2f)

#--cctrt and crop are always sig, the others change depend on model

#--heterogeneous variance should be hypothesis driven. Why would it vary?
#--look at variances btwn blocks
ggplot(data = data.frame(resid = residuals(m2d), 
                         block_id = d$block_id)) + 
  geom_boxplot(aes(x = block_id, y = resid)) + 
  theme_minimal()

#--look at variances btwn cc plots
ggplot(data = data.frame(resid = residuals(m2d), 
                         cctrt_id = d$cctrt_id)) + 
  geom_boxplot(aes(x = cctrt_id, y = resid)) + 
  theme_minimal()

#--I have no physical reason to assume variance would differ,
#--therefore don't include it

#--use glmmTMB notation
# mod_het_TMB <- glmmTMB(Y ~ W_Treat * S_Treat * SS_Treat + 
#                          (1 | Block) + (1 | Block:W_Treat) + (1 | Block:W_Treat:S_Treat),
#                        data = mydata,
#                        dispformula = ~ W_Treat) 

anova(m1a) #--p values of 1?
anova(m1b) #--more sensical
anova(m1c) #--only crop and cctrt
anova(m2d) #--only crop and cctrt
AIC(m1c, m2d) #--these should be the same model

#--model c/d seems to make more sense
AIC(m1c, m1b)
AIC(m1c, m2d)
#--model c or d has lower AIC

#--Maarit says don't worry about what the DHARMa is flagging
library(DHARMa)
plot(simulateResiduals(m1c))
plot(m1c)
#--this looks good, the pattern is fine
qqnorm(resid(m1c))
qqline(resid(m1c))

mwin <- m2c
anova(mwin)
tidy(anova(mwin))

#--marginal means
em_crop <- emmeans(mwin, ~crop)
em_cc <- emmeans(mwin, ~cctrt_id)

#--radM is higher than all of them, all others equal
tidy(pairs(emmeans(mwin, ~ cctrt_id))) %>% 
  filter(adj.p.value < .05)

em_cc_means <- 
  tidy(em_cc) %>% 
  arrange(-estimate) %>%  
  mutate(letter_sig = c("a", "b", "b", "b", "b"),
         resp_var = "crop_yield") 

em_cc_means %>% 
  write_csv("data/em_crop-yields-by-cc.csv")

r <- 
  as_tibble(anova(mwin)) %>% 
  mutate(term = row.names(anova(mwin))) %>% 
  select(term, everything())

r %>% 
  write_csv("data/stats_yields.csv")



#--effect of crop, but no interactions
#--effect of cover crop, no interactions

# #--should present it as cover crop as that is the focus

# # cover crops -------------------------------------------------------------
# em_cc
# radM <- c(0, 0, 0, 0, 1)
# mixes <- c(0.5, 0.5, 0, 0, 0)
# contrast(em_cc, method = list("radM - mixes" = radM - mixes))
