# created 5 nov 2024
# purpose: do stats on fall biomass
# notes: It is not clear if in 2019 the volunteers were omitted, 
# -- or if they got lumped in another cat
# -- Bo says they got lumped in 'non crop' category
# -- must combine volunteer and weeds data into 'non-cover crop'
#--23 april 2024 update, don't run separate models for each year except to get letters
#--working through fixing that...

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

m3 <- glmmTMB(dm_gm2 ~ till_id * cctrt_id * straw_id * weayear + 
                (1 | block_id/straw_id/till_id/cctrt_id), 
              #dispformula = ~crop,
              data = dtot)

#--variance differs by crop
m4 <- lme(dm_gm2 ~ till_id * straw_id * cctrt_id * weayear,
          random = ~1 | block_id/straw_id/till_id/cctrt_id,
          data = dtot,
          weights = varIdent(form = ~1 | weayear))

anova(m4)
plot(simulateResiduals(m4))
plot(m4)
#--this looks good, the pattern is fine
qqnorm(resid(m4))
qqline(resid(m4))


anova(m4) |> 
  rownames_to_column() |> 
  as_tibble() |> 
  write_xlsx("data/stats/anova/anova_fallbio.xlsx")


# straw -------------------------------------------------------------------
#--straw effect sig, no interactions, results are in g/m2
#--1/1 000 * 1/1 000 * 10 000 == divide by 100 to get Mg/ha

summary(emmeans(m4, specs = ~straw_id, type = "response"))
emmeans(m4, specs = pairwise~straw_id)$contrasts

# tillage -------------------------------------------------------------------
#--effect depended on year, and also on cover crop, but not an intxn

summary(emmeans(m4, specs = ~till_id:weayear, type = "response"))
189/286
82/106

summary(emmeans(m4, specs = ~till_id, type = "response"))
135/196

#--the difference between years was stronger in no-till than the other two tillages
emmeans(m4, specs = pairwise~till_id:weayear)$contrasts %>% 
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
summary(emmeans(m4, specs = ~cctrt_id:weayear, type = "response"))
188/278
63/134

summary(emmeans(m4, specs = ~cctrt_id, type = "response"))
emmeans(m4, specs = pairwise~cctrt_id)$contrasts %>% 
  tidy(.) %>% 
  filter(adj.p.value < 0.05)

#--the difference between years was stronger xx cctrt
emmeans(m4, specs = pairwise~cctrt_id:weayear)$contrasts %>% 
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

emmeans(m4, specs = pairwise~till_id:cctrt_id)$contrasts %>% 
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

summary(emmeans(m4, specs = ~till_id, type = "response")) %>% 
  tidy() %>% 
  write_csv("data/tables/em_fbio-tillage.csv")

summary(emmeans(m4, specs = ~cctrt_id, type = "response")) %>% 
  tidy() %>% 
  write_csv("data/tables/em_fbio-cctrt.csv")

summary(emmeans(m4, specs = ~straw_id, type = "response")) %>% 
  tidy() %>% 
  write_csv("data/tables/em_fbio-straw.csv")

summary(emmeans(m4, specs = ~weayear, type = "response")) %>% 
  tidy() %>% 
  write_csv("data/tables/em_fbio-wea.csv")


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
  
#--mixM retained, biomass was greater in surface than in inversion
d_19 %>% 
  filter(cctrt_id == "mix_M") %>% 
  filter(straw_id == "retained") %>% 
  ggplot(aes(till_id, dm_gm2)) +
  geom_point(aes(colour = block_id))

d_19 %>% 
  filter(cctrt_id == "mix_M") %>% 
  filter(straw_id == "retained") %>% 
  filter(block_id == "b2")

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

