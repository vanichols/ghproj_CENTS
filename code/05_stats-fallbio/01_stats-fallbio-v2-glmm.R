# created 5 nov 2024
# purpose: do stats on fall biomass
# notes: It is not clear if in 2019 the volunteers were omitted, 
# -- or if they got lumped in another cat
# -- Bo says they got lumped in 'non crop' category
# -- must combine volunteer and weeds data into 'non-cover crop'


library(tidyverse)
library(lme4)
library(lmerTest)
library(CENTSdata)
library(broom)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(car)

library(multcomp)

rm(list = ls())

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_fallbio)
c <- 
  as_tibble(cents_cropyields) %>% 
  mutate(year = year(date2)) %>% 
  dplyr::select(year, crop)

w <- 
  read_csv("data/tidy_weaclass.csv") %>% 
  unite(col = "weayear", precip, te, sep  = "/") %>%
  left_join(c) %>% 
  mutate(weayear = paste(year, weayear, crop, sep = ", ")) %>% 
  dplyr::select(-crop)


#--data
d <- 
  eu %>% 
  left_join(y) %>% 
  mutate(year = year(date2)) %>% 
  left_join(w) %>% 
  dplyr::select(weayear, everything()) %>% 
  distinct()

# viz ---------------------------------------------------------------------

#--there are a fair number of zeros in the data
d %>% 
  ggplot(aes(dm_gm2)) +
  geom_histogram()

#--it is mostly from the weeds category in 2018
d %>% 
  ggplot(aes(dm_gm2)) +
  geom_histogram() +
  facet_grid(year~dm_type)

#--pretty evenly dist throughout the treatments
d %>% 
  ggplot(aes(dm_gm2)) +
  geom_histogram() +
  facet_grid(cctrt_id~dm_type)

d %>% 
  ggplot(aes(dm_type, dm_gm2)) +
  geom_point() +
  facet_grid(.~year) +
  labs(title = "Fall dry matter")


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
  group_by(subplot_id, date2, year, dm_type2) %>% 
  summarise(dm_gm2 = sum(dm_gm2, na.rm = T)) %>% 
  left_join(eu) %>% 
  left_join(w) %>% 
  distinct()

#--they are not strongly related
d1 %>% 
  pivot_wider(names_from = dm_type2, values_from = dm_gm2)  %>% 
  left_join(eu) %>% 
  ggplot(aes(covercrop, other)) +
  geom_point(aes(color = cctrt_id))

# model on total biomass-------------------------------------------------------------------

dtot <- 
  d1 %>% 
  group_by(subplot_id, date2, year, eu_id, block_id, plot_id, till_id, 
           rot_id, straw_id, cctrt_id, weayear) %>% 
  summarise(dm_gm2 = sum(dm_gm2, na.rm = T))

#--same model as for crop yields

m1 <- lmer(dm_gm2 ~ till_id * cctrt_id * straw_id * weayear + 
             (1|block_id),
           #(1|cctrt_id:till_id:straw_id) + 
           #(1|till_id:straw_id) +
           #(1|straw_id), 
           data = dtot)

summary(m1)
anova(m1)
plot(simulateResiduals(m1))
plot(m1)
#--this looks good, the pattern is fine
qqnorm(resid(m1))
qqline(resid(m1))


# fit separately by year --------------------------------------------------
d_18 <- 
  dtot %>% 
  filter(year == 2018)

d_18 %>% 
  ggplot(aes(dm_gm2)) +
  geom_histogram()
#--see which of the terms may be able to get dropped, straw doesn't have much varation 
d_18 %>% 
  ggplot(aes(till_id, dm_gm2)) +
  geom_boxplot()

d_18 %>% 
  ggplot(aes(cctrt_id, dm_gm2)) +
  geom_boxplot()

#--straw not really
d_18 %>% 
  ggplot(aes(straw_id, dm_gm2)) +
  geom_boxplot()

#--block maybe
d_18 %>% 
  ggplot(aes(block_id, dm_gm2)) +
  geom_boxplot()

d_18 %>% 
  filter(dm_gm2 == 0)

m_18 <- glmmTMB(dm_gm2 ~ till_id * cctrt_id * straw_id +
                       (1|block_id) + 
                       (1|cctrt_id:till_id),# + 
                       #(1|till_id),
                     #family=Gamma(link="log"), 
                     data= d_18)

m_18a <- update(m_18, family = Gamma(link = "log"))
#--it is not a better fit, but it doesn't work in emmeans and I don't know why
AIC(m_18a, m_18)

plot(simulateResiduals(m_18))
plot(m_18)

#--tillage and cctrt interact within 2018
summary(m_18)
warnings()
Anova(m_18, type = "III")

em_18 <- emmeans(m_18, specs = ~cctrt_id:till_id, type = "response")
summary(em_18)

res_18 <- 
  tidy(multcomp::cld(em_18, Letters = LETTERS)) %>% 
  mutate(year = 2018)
#--2019

d_19 <- 
  dtot %>% 
  filter(year == 2019)

m_19 <- glmmTMB(dm_gm2 ~ till_id * cctrt_id * straw_id +
                  (1|block_id) + 
                  (1|cctrt_id:till_id),# + 
                  #(1|till_id),
                #family=binomial(link="logit"), 
                data= d_19)


#--tillage and cctrt and straw interact within 2019
summary(m_19)
Anova(m_19, type = "III")

emmeans(m_18, specs = ~till_id)
#--note that MixM in the non-inversion is the only instance
#--where straw removal made a sig difference
em_19 <- emmeans(m_19, specs = ~cctrt_id:till_id:straw_id)

tst <- 
  emmeans(m_19, specs = pairwise~cctrt_id:till_id:straw_id)$contrasts %>% 
  tidy(.) %>% 
  filter(adj.p.value < 0.05) %>% 
  separate(contrast, into = c("x1", "x2"), sep = "-") %>% 
    mutate(x1 = str_squish(x1),
           x2 = str_squish(x2)) %>% 
    separate(x1, into = c("c1a", "c1b", "c1c"), sep = " ") %>% 
    separate(x2, into = c("c2a", "c2b", "c2c"), sep = " ") %>% 
  filter(c1a == c2a)
  

#--for displaying in a readable way, ignore that interaction but make a note
em_19 <- emmeans(m_19, specs = ~cctrt_id:till_id)

res_19 <- 
  tidy(multcomp::cld(em_19, Letters = LETTERS)) %>% 
  mutate(year = 2019)


res_19 %>% 
  ggplot(aes(till_id, estimate)) +
  geom_col() +
  geom_label(aes(label = .group)) +
  facet_grid(. ~ cctrt_id)

res <- 
  res_18 %>% 
  bind_rows(res_19)

res %>% 
  write_csv("data/stats_em_fallbio.csv")
  

####################stopped####LETTERS################stopped
# modelb <- emmeans(fit1, c("Farm","Time"), type="response")
# multcomp::cld(modelb, Letters = LETTERS, alpha=0.05)
# 
# 
# # full model straw -------------------------------------------------------------------
# 
# emmeans(m1, specs = pairwise ~straw_id:weayear) 
# emmeans(m1, specs = ~weayear) 


# tillage impacts ---------------------------------------------------------

emmeans(m_18, specs = ~till_id) 
em_m18till <- emmeans(m_18, specs = pairwise ~ till_id) 
(286-208)/286
(208-189)/208
(286-189)/286

#--compare no-till to other two tillages for each
notill <- c(0, 0, 1)
others <- c(0.5, 0.5, 0)

contrast(em_m18till, method = list("notill - all" = notill - others))
29.6

# 
# 
# # full model tillage ------------------------------------------------------
# 
# a <- 
#   tidy(emmeans(m1, specs = pairwise ~till_id:weayear)$contrasts) %>% 
#   separate(contrast, into = c("v1", "v2"), sep = "-")
# 
# a18 <- 
#   a %>% 
#   filter(grepl("2018", v1) & grepl("2018", v2))
# 
# a19 <- 
#   a %>% 
#   filter(grepl("2019", v1) & grepl("2019", v2))
# 
# 
# # full model till x cctrt -------------------------------------------------
# 
# txc <- 
#   tidy(emmeans(m1, specs = pairwise ~till_id:cctrt_id)$contrasts) %>% 
#   separate(contrast, into = c("v1", "v2"), sep = "-")
# 
# #--radish late
#   txc %>% 
#   filter(grepl("rad_L", v1) & grepl("rad_L", v2))
# 
# #--radish mid
# txc %>% 
#   filter(grepl("rad_M", v1) & grepl("rad_M", v2))
# 
# 
# #--nocc vs radish mid
# zzz<- 
#   txc %>% 
#   filter(grepl("nocc", v1) & grepl("rad_M", v2))
# 
# #--mix e
# txc %>% 
#   filter(grepl("mix_E", v1) & grepl("mix_E", v2))
# 
# #--mix m
# txc %>% 
#   filter(grepl("mix_M", v1) & grepl("mix_M", v2))
# 
# #--mix e
# txc %>% 
#   filter(grepl("mix_E", v1) & grepl("mix_E", v2))
# 
# #--nocc
# zzz<-txc %>% 
#   filter(grepl("nocc", v1) & grepl("nocc", v2))
# 
# #--notill
# zzz <- txc %>% 
#   filter(grepl("notill", v1) & grepl("notill", v2))
# 
# #--noninv
# zzz <- txc %>% 
#   filter(grepl("non", v1) & grepl("non", v2))
# 
# 
# # full model cc ------------------------------------------------------
# 
# b <- 
#   tidy(emmeans(m1, specs = pairwise ~cctrt_id:weayear)$contrasts) %>% 
#   separate(contrast, into = c("v1", "v2"), sep = "-")
# 
# b18 <- 
#   b %>% 
#   filter(grepl("2018", v1) & grepl("2018", v2)) %>% 
#   arrange(estimate)
# 
# b19 <- 
#   b %>% 
#   filter(grepl("2019", v1) & grepl("2019", v2))%>% 
#   arrange(estimate)
# 
# 
# # run separately for each year --------------------------------------------
# 
# m2018 <- lmer(dm_gm2 ~ till_id * cctrt_id * straw_id + 
#              (1|block_id),
#            #(1|cctrt_id:till_id:straw_id) + 
#            #(1|till_id:straw_id) +
#            #(1|straw_id), 
#            data = dtot %>% filter(year == 2018))
# 
# m2019 <- lmer(dm_gm2 ~ till_id * cctrt_id * straw_id + 
#                 (1|block_id),
#               #(1|cctrt_id:till_id:straw_id) + 
#               #(1|till_id:straw_id) +
#               #(1|straw_id), 
#               data = dtot %>% filter(year == 2019))
# 
# #--no interactions
# tidy(anova(m2018)) %>% 
#   filter(p.value < 0.05)
# 
# #--no interactions
# tidy(anova(m2019)) %>% 
#   filter(p.value < 0.05)
# 
# emmeans(m2019, specs = pairwise ~ till_id) 
# emmeans(m2019, specs = pairwise ~ till_id) 
# 
# # main effects -------------------------------------------------------------
# 
# ex <- emmeans(m2018, specs = ~ cctrt_id) 
# cld(ex)
# 
# #--2018
# ex2 <- emmeans(m2018, specs = ~ cctrt_id:till_id:straw_id, alpha = 0.01) 
# res2 <- tidy(cld(ex2))
# 
# 
# res2 %>% 
#   arrange(estimate) %>% 
#   mutate(group = as.factor(.group),
#          trt = paste(cctrt_id, till_id, straw_id)) %>%
#   ggplot(aes(trt, estimate)) +
#   geom_point(aes(color = group), size = 5) +
#   coord_flip() +
#   facet_grid(till_id + straw_id ~ ., scales = "free_y")
# 
# 
# #--2019
# ex3 <- emmeans(m2019, specs = ~ cctrt_id:till_id:straw_id, alpha = 0.01) 
# res3 <- tidy(cld(ex3))
# 
# 
# res3 %>% 
#   arrange(estimate) %>% 
#   mutate(group = as.factor(.group),
#          trt = paste(cctrt_id, till_id, straw_id)) %>%
#   ggplot(aes(trt, estimate)) +
#   geom_point(aes(color = group), size = 5) +
#   coord_flip() +
#   facet_grid(till_id + straw_id ~ ., scales = "free_y")
# 
# 
# emmeans(m2018, specs = ~ cctrt_id) %>% 
#   tidy() %>% arrange(estimate)
# 
# emmeans(m1, specs = ~ cctrt_id)
# 
# em_cc <- emmeans(m1, specs = pairwise ~ cctrt_id)
# em_cc_means <- tidy(em_cc$emmeans) 
# 
# em_cc_means %>% 
#   mutate(letter_sig = c("a", "a", "ab", "ab", "b")) %>% 
#   ggplot(aes(cctrt_id, estimate)) +
#   geom_errorbar(aes(x = cctrt_id, ymin = estimate - std.error, ymax = estimate + std.error),
#                 width = 0.2) +
#   geom_point() +
#   geom_text(aes(x = cctrt_id, y = estimate + 0.3, label = letter_sig)) +
#   scale_y_continuous(limits = c(0, 4.5))
# 
# em_cc
# radM <- c(0, 0, 0, 0, 1)
# mixes <- c(0.5, 0.5, 0, 0, 0)
# contrast(em_cc, method = list("radM - mixes" = radM - mixes))
# 
# 
# # tillage by straw --------------------------------------------------------
# 
# em_ts <- emmeans(m2, specs = pairwise ~ till_id:straw_id)
# 
# em_ts$contrasts
# 
# #--when removing straw, no-till yields less than the other tillages
# #--when retaining straw, no difference in yields
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
