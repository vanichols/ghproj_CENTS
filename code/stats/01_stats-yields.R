# created 15 oct 2024
# purpose: do stats on crop yields, using new CENTSdata package
# notes: starting with basic response variable w/maarit's help

library(tidyverse)
library(lme4)
library(lmerTest)
library(CENTSdata)
library(broom)
library(emmeans)

rm(list = ls())

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_cropyields)

d <- eu %>% left_join(y)


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

m2 <- lmer(yield_dry_Mgha ~ till_id * cctrt_id * straw_id * crop + 
             (1|block_id),
             #(1|cctrt_id:till_id:straw_id) + 
             #(1|till_id:straw_id) +
             #(1|straw_id), 
           data = d)

summary(m2)
anova(m2)
#--don't worry about what the DHARMa is flagging
library(DHARMa)
plot(simulateResiduals(m1))
plot(m2)
#--this looks good, the pattern is fine
qqnorm(resid(m2))
qqline(resid(m2))

anova(m2)

r <- tidy(anova(m2)) 

r %>% 
  write_csv("data/stats_yields.csv")

r %>% 
  filter(p.value < 0.05)


r %>% 
  filter(grepl("crop", term)) %>% 
  filter(term != "crop")

#--effect of crop, but no interactions
#--effect of cover crop
#--effect of tillage, depending on straw


# cover crops -------------------------------------------------------------
em_cc <- emmeans(m2, specs = pairwise ~ cctrt_id)
em_cc_means <- tidy(em_cc$emmeans) 

em_cc_means %>% 
  mutate(letter_sig = c("a", "a", "ab", "ab", "b")) %>% 
  ggplot(aes(cctrt_id, estimate)) +
  geom_errorbar(aes(x = cctrt_id, ymin = estimate - std.error, ymax = estimate + std.error),
                width = 0.2) +
  geom_point() +
  geom_text(aes(x = cctrt_id, y = estimate + 0.3, label = letter_sig)) +
  scale_y_continuous(limits = c(0, 4.5))

em_cc
radM <- c(0, 0, 0, 0, 1)
mixes <- c(0.5, 0.5, 0, 0, 0)
contrast(em_cc, method = list("radM - mixes" = radM - mixes))


# tillage by straw --------------------------------------------------------

em_ts <- emmeans(m2, specs = pairwise ~ till_id:straw_id)

em_ts$contrasts

#--when removing straw, no-till yields less than the other tillages
#--when retaining straw, no difference in yields

#--compare no-till to other two tillages for each 
notillnostraw <- c(0, 0, 1, 0, 0, 0)
othersnostraw <- c(0.5, 0.5, 0, 0, 0, 0)


contrast(em_ts, method = list("notill - all" = notill - othersnostraw) )

# emmeans for all (except crop)---------------------------------------------------------

em_all <- tidy(emmeans(m2, specs = pairwise ~ till_id|straw_id|cctrt_id)$emmeans) 

em_all %>% 
  ggplot(aes(straw_id, estimate)) +
  geom_errorbar(aes(x = straw_id, ymin = estimate - std.error, ymax = estimate + std.error, 
                    color = cctrt_id),
                width = 0.2) +
  geom_point(aes(color = cctrt_id)) +
  facet_grid(.~till_id) +
  scale_y_continuous(limits = c(0, 4.7))
