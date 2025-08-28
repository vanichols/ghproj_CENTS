# created 15 oct 2024
# purpose: do stats on crop yields, using new CENTSdata package
# notes: starting with basic response variable w/maarit's help
#--1 april 2025 - cleaned up and picked a model

library(tidyverse)
library(lme4)
library(lmerTest)
library(CENTSdata)
library(broom)
library(emmeans)
library(glmmTMB)
library(broom.mixed)

rm(list = ls())


# DK yields ---------------------------------------------------------------

readxl::read_excel("data/dkstats-small-grain-yields-over-time.xlsx", 
                   skip = 2) %>% 
  select(6:ncol(.)) %>% 
  slice(1:6) %>% 
  janitor::clean_names() %>%
  mutate(x2009 = as.numeric(x2009)) %>% 
  pivot_longer(2:ncol(.), names_to = "year", values_to = "yield") %>% 
  mutate(year = str_remove(year, "x"),
         year = as.numeric(year)) %>% 
  filter(year > 2017) %>% 
  group_by(crop) %>% 
  summarise(yield = mean(yield)/10)

dk_y <- 
  readxl::read_excel("data/dkstats-small-grain-yields-over-time.xlsx", 
                   skip = 2) %>% 
  select(6:ncol(.)) %>% 
  slice(1:6) %>% 
  janitor::clean_names() %>%
  mutate(x2009 = as.numeric(x2009)) %>% 
  pivot_longer(2:ncol(.), names_to = "year", values_to = "yield") %>% 
  mutate(year = str_remove(year, "x"),
         year = as.numeric(year)) 

dk_y %>% 
  group_by(crop) %>% 
  summarise(yield = mean(yield, na.rm = T)/10)

#--barley
dk_y %>% 
  filter((crop == "Spring barley" & year == 2018)) %>% 
  summarise(yield = mean(yield)/10)

#--oats
dk_y %>% 
  filter((crop == "Oats, mixed grains and other grains" & year == 2019)) %>% 
  summarise(yield = mean(yield)/10)

#--faba
dk_y %>% 
  filter((crop == "faba beans" & year == 2020)) %>% 
  summarise(yield = mean(yield)/10)

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

mwin <- m1c
anova(mwin)
tidy(anova(mwin))

#--marginal means
em_crop <- emmeans(mwin, ~crop)
em_cc <- emmeans(mwin, ~cctrt_id)

radM <- c(0, 0, 0, 0, 1)
other <- c(0.25, 0.25, 0.25, 0.25, 0)
contrast(em_cc, method = list("radM - other" = radM - other))
.324/4.20
0.0734/4.20

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

