# created 28 aug 2025
# purpose: execute final stats models, get estimates needed for manu

library(CENTSdata)
library(tidyverse)

library(lme4)
library(lmerTest)
library(broom)
library(emmeans)
library(glmmTMB)
library(broom.mixed)
#--this one does letters
library(multcomp)

#--note: use dplyr::select bc of conflict issues

rm(list = ls())

#--helpful emmeans website: https://data-wise.github.io/doe/appendix/r-packages/emmeans.html

# 1. data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)



# 2. yields ---------------------------------------------------------------

#--data
d2a <- as_tibble(cents_cropyields)

d2 <- 
  eu %>% 
  left_join(d2a) %>% 
  mutate(estimate = yield_dry_Mgha)

#--checking data

d2 %>% 
  mutate(year = lubridate::year(date2)) %>% 
  ggplot(aes(crop, yield_dry_Mgha)) +
  geom_jitter(aes(shape = till_id, color = cctrt_id))

d2 %>% 
  group_by(crop) %>% 
  summarise(minY = min(yield_dry_Mgha),
            maxY = max(yield_dry_Mgha))

d2 %>% 
  ggplot(aes(crop, yield_dry_Mgha)) +
  geom_jitter()

#--model (chosen from exp-yields code)
m2 <- lmer(yield_dry_Mgha ~ cctrt_id * till_id * straw_id * cctrt_id * crop + 
              (1|block_id) +
              #(1|block_id:straw_id) +
              (1|block_id:straw_id:till_id),
            #(1|till_id:straw_id) +
            #(1|straw_id), 
            data = d2)

#--anova on full model
aov2 <- 
  as_tibble(anova(m2)) %>% 
  mutate(term = row.names(anova(m2))) %>% 
  dplyr::select(term, everything()) %>% 
  janitor::clean_names()

aov2 %>% 
  write_csv("data/stats_anova/anova_yield.csv")

#--impact of crop (duh), cctrt, no interaction
aov2 %>% 
  filter(pr_f < 0.05)
  

#--marginal means
em2_crop <- emmeans(m2, ~crop)
em2_cc <- emmeans(m2, ~cctrt_id)

#--radM is higher than all of them, all others equal
tidy(pairs(emmeans(m2, ~ cctrt_id))) %>% 
  filter(adj.p.value < .05)

#--letters (consistent with above)
l2_cc <- 
  tidy(multcomp::cld(em2_cc, Letters = LETTERS)) %>% 
  mutate(resp_var = "crop_yield")

l2_cc %>% write_csv("data/stats_letters/letters_yield_by-cc.csv")

#--contrasts
radM <- c(0, 0, 0, 0, 1)
other <- c(0.25, 0.25, 0.25, 0.25, 0)
contrast(em2_cc, method = list("radM - other" = radM - other))
tidy(em2_cc) %>% 
  filter(cctrt_id != "rad_M") %>% 
  summarise(estimate = mean(estimate))

.324/3.87 #--0.324 Mg is an increase of 8%


# 3. fall biomass ---------------------------------------------------------

d3a <- 
  as_tibble(cents_fallbio) %>% 
  mutate(year = paste0("Y", year(date2)))

##--combine weeds and volunteers
d3b <- 
  d3a %>% 
  mutate(dm_type2 = case_when(
    dm_type == "volunteer" ~ "other",
    dm_type == "weeds" ~ "other",
    dm_type == "grass_cl" ~ "covercrop",
    dm_type == "radish" ~ "covercrop",
    TRUE ~ dm_type)
  ) %>% 
  group_by(eu_id, date2, year, dm_type2) %>% 
  summarise(dm_gm2 = sum(dm_gm2, na.rm = T)) %>% 
  left_join(eu) %>% 
  distinct()

#--get total biomass
d3 <- 
  d3b %>% 
  group_by(eu_id, date2, year, block_id, plot_id, till_id, 
           rot_id, straw_id, cctrt_id) %>% 
  summarise(dm_gm2 = sum(dm_gm2, na.rm = T))


#--same model as for crop yields
m3 <- lmer(dm_gm2 ~ till_id * cctrt_id * straw_id * year + 
             (1|block_id),
           #(1|cctrt_id:till_id:straw_id) + 
           #(1|till_id:straw_id) +
           #(1|straw_id), 
           data = d3)

#--anova on full model
aov3 <- 
  as_tibble(anova(m3)) %>% 
  mutate(term = row.names(anova(m3))) %>% 
  dplyr::select(term, everything()) %>% 
  janitor::clean_names()

aov3 %>% 
  write_csv("data/stats_anova/anova_totbio.csv")

aov3 %>% 
  filter(pr_f < 0.05)
#--impact of everything, basically, but straw is minor

#--run separate model just to get letters that can be interpreted
m3_18 <- glmmTMB(dm_gm2 ~ till_id * cctrt_id * straw_id +
                  (1|block_id) + 
                  (1|cctrt_id:till_id),
                data= d3 %>% filter(year == "Y2018"))

em3_18 <- emmeans(m3_18, specs = ~cctrt_id:till_id, type = "response")
l3_18 <- 
  tidy(multcomp::cld(em3_18, Letters = LETTERS)) %>% 
  mutate(year = 2018,
         resp_var = "tot_bio")

m3_19 <- glmmTMB(dm_gm2 ~ till_id * cctrt_id * straw_id +
                   (1|block_id) + 
                   (1|cctrt_id:till_id),
                 data= d3 %>% filter(year == "Y2019"))

em3_19 <- emmeans(m3_19, specs = ~cctrt_id:till_id, type = "response")
l3_19 <- 
  tidy(multcomp::cld(em3_19, Letters = LETTERS)) %>% 
  mutate(year = 2019,
         resp_var = "tot_bio")

l3_18 %>% 
  bind_rows(l3_19) %>% 
  write_csv("data/stats_letters/letters_totbio-by-year.csv")


# straw -------------------------------------------------------------------
#--straw effect sig, no interactions, results are in g/m2
#--1/1 000 * 1/1 000 * 10 000 == divide by 100 to get Mg/ha

summary(emmeans(m1, specs = ~straw_id, type = "response"))
emmeans(m1, specs = pairwise~straw_id)$contrasts

# tillage -------------------------------------------------------------------
#--effect depended on year, and also on cover crop, but not an intxn

summary(emmeans(m1, specs = ~till_id:weayear, type = "response"))
189/286
82/106

summary(emmeans(m1, specs = ~till_id, type = "response"))
135/196

#--the difference between years was stronger in no-till than the other two tillages
emmeans(m1, specs = pairwise~till_id:weayear)$contrasts %>% 
  tidy(.) %>% 
  filter(adj.p.value < 0.05) %>% 
  separate(contrast, into = c("x1", "x2"), sep = "-") %>% 
  mutate(x1 = str_squish(x1),
         x2 = str_squish(x2)) %>% 
  separate(x1, into = c("c1a", "c1b", "c1c"), sep = " ") %>% 
  separate(x2, into = c("c2a", "c2b", "c2c"), sep = " ") %>% 
  filter(c1b != c2b) %>% 
  filter(c1a == c2a)


# cctrt -------------------------------------------------------------------
#--effect depended on year, and also on tillage, but not an intxn
summary(emmeans(m1, specs = ~cctrt_id:weayear, type = "response"))
188/278
63/134

summary(emmeans(m1, specs = ~cctrt_id, type = "response"))
emmeans(m1, specs = pairwise~cctrt_id)$contrasts %>% 
  tidy(.) %>% 
  filter(adj.p.value < 0.05)

#--the difference between years was stronger xx cctrt
emmeans(m1, specs = pairwise~cctrt_id:weayear)$contrasts %>% 
  tidy(.) %>% 
  filter(adj.p.value < 0.05) %>% 
  separate(contrast, into = c("x1", "x2"), sep = "-") %>% 
  mutate(x1 = str_squish(x1),
         x2 = str_squish(x2)) %>% 
  separate(x1, into = c("c1a", "c1b", "c1c"), sep = " ") %>% 
  separate(x2, into = c("c2a", "c2b", "c2c"), sep = " ") %>% 
  filter(c1b != c2b) %>% 
  filter(c1a == c2a) %>% 
  arrange(estimate) %>% 
  dplyr::select(-c1c, -c2c)



# till x cctrt ------------------------------------------------------------

emmeans(m1, specs = pairwise~till_id:cctrt_id)$contrasts %>% 
  tidy(.) %>% 
  filter(adj.p.value < 0.05) %>% 
  separate(contrast, into = c("x1", "x2"), sep = "-") %>% 
  mutate(x1 = str_squish(x1),
         x2 = str_squish(x2)) %>% 
  separate(x1, into = c("c1a", "c1b"), sep = " ") %>% 
  separate(x2, into = c("c2a", "c2b"), sep = " ") %>% 
  filter(c1b == c2b) %>% 
  filter(c1a == c2a)



# make marginal mean tables for supp material -----------------------------

summary(emmeans(m1, specs = ~till_id, type = "response")) %>% 
  tidy() %>% 
  write_csv("data/tables/em_fbio-tillage.csv")

summary(emmeans(m1, specs = ~cctrt_id, type = "response")) %>% 
  tidy() %>% 
  write_csv("data/tables/em_fbio-cctrt.csv")

summary(emmeans(m1, specs = ~straw_id, type = "response")) %>% 
  tidy() %>% 
  write_csv("data/tables/em_fbio-straw.csv")

summary(emmeans(m1, specs = ~weayear, type = "response")) %>% 
  tidy() %>% 
  write_csv("data/tables/em_fbio-wea.csv")

