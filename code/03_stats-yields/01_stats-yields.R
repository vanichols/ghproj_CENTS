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

m2a <- lmer(yield_dry_Mgha ~ till_id * cctrt_id * straw_id * crop + 
             (1|block_id) +
             (1|cctrt_id:till_id:straw_id), #+ 
             #(1|till_id:straw_id) +
             #(1|straw_id), 
           data = d)

m2b <- lmer(yield_dry_Mgha ~ till_id * cctrt_id * straw_id * crop + 
              (1|block_id),# +
              #(1|cctrt_id:till_id:straw_id), #+ 
            #(1|till_id:straw_id) +
            #(1|straw_id), 
            data = d)

m2c <- lmer(yield_dry_Mgha ~ cctrt_id * till_id * straw_id * cctrt_id * crop + 
              (1|block_id) +
              #(1|block_id:straw_id) +
              (1|block_id:straw_id:till_id),
            #(1|till_id:straw_id) +
            #(1|straw_id), 
            data = d)


library(nlme)
# model <- lme(Y ~ W_Treat * S_Treat * SS_Treat, 
#              random = ~1 | Block/W_Treat/S_Treat, 
#              data = mydata)

m2d <- lme(yield_dry_Mgha ~ cctrt_id * till_id * straw_id * cctrt_id * crop,
           random = ~1 | block_id/straw_id/till_id,
            data = d)

m2e <- lme(yield_dry_Mgha ~ cctrt_id * till_id * straw_id * cctrt_id * crop,
           random = ~1 | block_id/straw_id/till_id,
           data = d,
           weights = varIdent(form = ~1 | block_id))

m2f <- lme(yield_dry_Mgha ~ cctrt_id * till_id * straw_id * cctrt_id * crop,
           random = ~1 | block_id/straw_id/till_id,
           data = d,
           weights = varIdent(form = ~1 | cctrt_id))

#--compare hetergenous variances
anova(m2d, m2e)
anova(m2d, m2f)
anova(m2e, m2f)
AIC(m2d, m2e, m2f)

#--look at variances btwn blocks, this seems like the best model
ggplot(data = data.frame(resid = residuals(m2d), 
                         block_id = d$block_id)) + 
  geom_boxplot(aes(x = block_id, y = resid)) + 
  theme_minimal()

#--look at variances btwn cc plots
ggplot(data = data.frame(resid = residuals(m2d), 
                         cctrt_id = d$cctrt_id)) + 
  geom_boxplot(aes(x = cctrt_id, y = resid)) + 
  theme_minimal()

#--use glmmTMB
mod_het_TMB <- glmmTMB(Y ~ W_Treat * S_Treat * SS_Treat + 
                         (1 | Block) + (1 | Block:W_Treat) + (1 | Block:W_Treat:S_Treat),
                       data = mydata,
                       dispformula = ~ W_Treat) 

anova(m2a)
anova(m2b)
anova(m2c)
anova(m2d)
#--model c seems to make more sense
AIC(m2c, m2b)
AIC(m2c, m2d) #--these should be the same model
#--model c or d has lower AIC

#--Maarit says don't worry about what the DHARMa is flagging
library(DHARMa)
plot(simulateResiduals(m2c))
plot(m2c)
#--this looks good, the pattern is fine
qqnorm(resid(m2c))
qqline(resid(m2c))



m2 <- m2e
anova(m2)
tidy(anova(m2))

em_crop <- emmeans(m2e, ~crop)

em_cc <- emmeans(m2e, ~cctrt_id)

tidy(pairs(emmeans(m2e, ~ cctrt_id))) %>% 
  filter(adj.p.value < .05)

em_cc_means <- 
  tidy(em_cc) %>% 
  arrange(-estimate) %>%  
  mutate(letter_sig = c("a", "ab", "b", "b", "b"),
         resp_var = "crop_yield") 

em_cc_means %>% 
  write_csv("data/em_crop-yields-by-cc.csv")

#-this is goofy now that I'm using nlme
anova(m2e)
r <- tidy(anova(m2e)) 

r %>% 
  write_csv("data/stats_yields.csv")


# 
# r %>% 
#   filter(grepl("crop", term)) %>% 
#   filter(term != "crop")
# 
# #--effect of crop, but no interactions
# #--effect of cover crop, no interactions
# #--effect of tillage, depending on straw
# 
# #--should present it as cover crop as that is the focus
# 
# # cover crops -------------------------------------------------------------
# em_cc <- emmeans(m2, specs = pairwise ~ cctrt_id)
# tidy(em_cc$contrasts) %>%
#   filter(adj.p.value < 0.05)
# #--nocc is lower than rad_M
# #--rad_L is lower than rad_M
# #--mix_M is lower than rad_M
# #--mixE is lower than radM
# 
# em_cc_means <- 
#   tidy(em_cc$emmeans) %>% 
#   arrange(-estimate) %>%  
#   mutate(letter_sig = c("a", "b", "b", "b", "b"),
#          resp_var = "crop_yield") 
# 
# em_cc_means %>% 
#   write_csv("data/em_crop-yields-by-cc.csv")
# 
# em_cc_means %>% 
#   ggplot(aes(cctrt_id, estimate)) +
#   geom_errorbar(aes(x = cctrt_id, ymin = estimate - std.error, ymax = estimate + std.error),
#                 width = 0.2) +
#   geom_point() +
#   geom_jitter(data = d, aes(cctrt_id, estimate, color = crop), width = 0.1) +
#   geom_text(aes(x = cctrt_id, y = estimate + 0.3, label = letter_sig)) +
#   scale_y_continuous(limits = c(0, 4.5))
# 
# 
# em_cc
# radM <- c(0, 0, 0, 0, 1)
# mixes <- c(0.5, 0.5, 0, 0, 0)
# contrast(em_cc, method = list("radM - mixes" = radM - mixes))
# 
# 
# # tillage by straw by crop--------------------------------------------------------
# 
em_ts <- emmeans(m2, specs = pairwise ~ till_id:straw_id:crop)

res_tsc <- 
  em_ts$contrasts %>% 
  tidy(.) %>% 
  filter(adj.p.value < 0.05)

res_tsc2 <- 
  res_tsc %>% 
  separate(contrast, sep = "-", into = c("C1", "C2")) %>% 
  mutate(C1 = str_squish(C1),
         C2 = str_squish(C2)) %>% 
  separate(C1, sep = " ", into = c("t1", "s1", "c1")) %>% 
  separate(C2, sep = " ", into = c("t2", "s2", "c2")) 


d %>% 
  filter(till_id == "notill", 
         straw_id == "removed") %>% 
  ggplot(aes(crop, yield_dry_Mgha)) +
  geom_point()

#--when removing straw, no-till yields less than the other tillages
#--when retaining straw, no difference in yields
# 
# #--compare no-till to other two tillages for each 
# notillnostraw <- c(0, 0, 1, 0, 0, 0)
# othersnostraw <- c(0.5, 0.5, 0, 0, 0, 0)
# 
# 
# contrast(em_ts, method = list("notill - all" = notill - othersnostraw) )
# 
# # emmeans for all (except crop)---------------------------------------------------------
# 
# em_all <- tidy(emmeans(m2, specs = pairwise ~ till_id|straw_id|cctrt_id)$emmeans) 
# 
# em_all %>% 
#   ggplot(aes(straw_id, estimate)) +
#   geom_errorbar(aes(x = straw_id, ymin = estimate - std.error, ymax = estimate + std.error, 
#                     color = cctrt_id),
#                 width = 0.2) +
#   geom_point(aes(color = cctrt_id)) +
#   facet_grid(.~till_id) +
#   scale_y_continuous(limits = c(0, 4.7))
