# created 5 nov 2024
# purpose: do stats on fall biomass
# notes: It is not clear if in 2019 the volunteers were omitted, 
# -- or if they got lumped in another cat


library(tidyverse)
library(lme4)
library(lmerTest)
library(CENTSdata)
library(broom)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(car)

rm(list = ls())

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_fallbio)
w <- read_csv("data/tidy_weaclass.csv")


#--data
d <- 
  eu %>% 
  left_join(y) %>% 
  mutate(year = year(date2)) %>% 
  left_join(w) %>% 
  unite(col = "weayear", precip, te, sep  = "-") %>% 
  select(weayear, everything())

#--total biomass
d_tot <-
  d %>% 
  group_by(weayear)

# viz ---------------------------------------------------------------------

#--there are a fair number of zeros in the data
d_cat %>% 
  ggplot(aes(cover_pct)) +
  geom_histogram()

#--it is mostly from the covercrop category
#--this makes sense since there is a nocover treatment
d_cat %>% 
  ggplot(aes(cover_pct)) +
  geom_histogram() +
  facet_grid(.~cover_cat)

#--indeed, it is mostly in the nocc, but also the mid_M
d_cat %>% 
  ggplot(aes(cover_pct)) +
  geom_histogram() +
  facet_grid(cctrt_id~cover_cat)

#--mostly in the dry-hot year
d_cat %>% 
  ggplot(aes(cover_pct)) +
  geom_histogram() +
  facet_grid(weayear~cover_cat)

### NEED TO ASK MAARIT ABOUT THIS

d_cc_order <- 
  d_sp %>% 
  arrange(cctrt_id) %>% 
  pull(cctrt_id) %>% 
  unique()

d_till_order <- 
  d_sp %>% 
  arrange(till_id) %>% 
  pull(till_id) %>% 
  unique()

#--kind of a nonsense fig, but to get the idea
d_sp %>% 
  arrange(date2, straw_id, till_id, cctrt_id) %>% 
  mutate(subplot_id = fct_inorder(subplot_id)) %>% 
  ggplot(aes(cctrt_id, cover_pct)) +
  geom_col(aes(fill = cover_cat)) +
  scale_x_discrete(labels = d_cc_order) +
  facet_grid(weayear~straw_id+till_id, scales = "free") +
  coord_flip() +
  scale_fill_manual(values = c("soil" = "brown4",
                               "covercrop" = "green3",
                               "other" = "gold"))

# it doesn't look like a dependence on tillage or straw removal
d_sp %>% 
  arrange(date2, straw_id, till_id, cctrt_id) %>% 
  mutate(subplot_id = fct_inorder(subplot_id)) %>% 
  ggplot(aes(till_id, cover_pct)) +
  geom_col(aes(fill = cover_cat)) +
  scale_x_discrete(labels = d_till_order) +
  facet_grid(weayear~straw_id + cctrt_id, scales = "free") +
  coord_flip() +
  scale_fill_manual(values = c("soil" = "brown4",
                               "covercrop" = "green3",
                               "other" = "gold"))

# model -------------------------------------------------------------------

# For the 4-way proportions for cover, you could actually analyse this a little di􀆯erently to what we talked about
# as well. You could pivot your data into long-format that has a column for the proportions, and another column
# for the Cover category (Soil, Weeds, Volunteers and CC). If you then analyse the proportions with a model like
# this:
  # model <- glmmTMB(proportion ~ CoverCategory * Tillage * CC * Straw * Year +
  #                    (1|Block) + (1|CC:Tillage:Straw) + (1|Tillage:Straw) + (1|Straw),
  #                  family=binomial(link=”logit”), data=DATA)
# You get an estimate of how di􀆯erent the proportions of each cover category are, and you are checking whether
# that is dependent on tillage, CC, straw or year e􀆯ects.

d

m1 <- glmmTMB(cover_frac ~ cover_cat * till_id * cctrt_id * straw_id * weayear +
                   (1|block_id) + 
                   (1|cctrt_id:till_id:straw_id) + 
                   (1|till_id:straw_id) + 
                   (1|straw_id),
                 family=binomial(link="logit"), 
              data=d_cat)

#--what if I only include two of the categories, 
#--since if you know two, the third is just 100 - their sum
m2 <- glmmTMB(cover_frac ~ cover_cat * till_id * cctrt_id * straw_id * weayear +
                (1|block_id) + 
                (1|cctrt_id:till_id:straw_id) + 
                (1|till_id:straw_id) + 
                (1|straw_id),
              family=binomial(link="logit"), 
              data=d_cat %>% filter(cover_cat != "other"))

#--these don't look good, but I'm not sure what to tweak
#--
m1_simres <- simulateResiduals(m1)
plot(m1_simres)

#--these look worse
m2_simres <- simulateResiduals(m2)
plot(m2_simres)

#--I think I ran out of DF
Anova(m1)

#--if I plow ahead, 
#--sig terms:
cover_cat:cctrt_id:weayear
cover_cat:weayear
cover_cat:cctrt_id 
cover_cat

#--nothing is interacting with tillage or straw removal, consistent w/perception
#--the effect of cctrt depends on the weayear
#--I think some cctrts are more reslient to weather conditions than others

em1_pairs <- emmeans(m1, specs = pairwise ~ cover_cat:cctrt_id:weayear)

#--I am not sure how to interpret these estimates on the logit scale
emmeans(m1, specs = ~ cover_cat:cctrt_id:weayear)
#--I think this back-transforms them
emmeans(m1, specs = ~ cover_cat:cctrt_id:weayear, type = "response")

em1_est <- tidy(emmeans(m1, specs = ~ cover_cat:cctrt_id:weayear, type = "response"))

em1_est %>% 
  ggplot(aes(cover_cat, prob)) +
  geom_jitter(aes(color = cctrt_id, shape = weayear), size = 4, width = 0.1)

#--correlation of modelled data
em1_est %>% 
  filter(cover_cat != "soil") %>% 
  select(cover_cat, cctrt_id, weayear, prob) %>% 
  pivot_wider(names_from = cover_cat, values_from = prob) %>% 
  ggplot(aes(covercrop, other)) +
  geom_point()


#--correlation of raw data
d_cat %>% 
  filter(cover_cat != "soil") %>% 
  select(cover_cat, cctrt_id, weayear, cover_frac) %>% 
  pivot_wider(names_from = cover_cat, values_from = cover_frac) %>% 
  ggplot(aes(covercrop, other)) +
  geom_point(aes(color = weayear))

#--comparing covercrop category in each weayear and cctrt

#--the early mix covercrop pctcover was strongly impacted by weather
r1 <- 
  tidy(em1_pairs$contrasts) %>% 
  separate(contrast, into = c("t1", "t2"), sep = " - ") %>% 
  filter(grepl("covercrop", t1)) %>% 
  filter(grepl("covercrop", t2)) %>% 
  separate(t1, into = c("covercat1", "cctrt_id1", "weayear1"), sep = " ") %>% 
  separate(t2, into = c("covercat2", "cctrt_id2", "weayear2"), sep = " ") %>% 
  filter(cctrt_id1 == cctrt_id2)
  
#--soil coverage was not impacted by cctrt
#--volunteers or weeds filled in what the cc did not
r2 <- 
  tidy(em1_pairs$contrasts) %>% 
  separate(contrast, into = c("t1", "t2"), sep = " - ") %>% 
  filter(grepl("soil", t1)) %>% 
  filter(grepl("soil", t2)) %>% 
  separate(t1, into = c("covercat1", "cctrt_id1", "weayear1"), sep = " ") %>% 
  separate(t2, into = c("covercat2", "cctrt_id2", "weayear2"), sep = " ") %>% 
  filter(cctrt_id1 == cctrt_id2)

#--only the mix_E 'other'category was impacted by weather
#--this is probably due to a lot of barley volunteers
r3 <- 
  tidy(em1_pairs$contrasts) %>% 
  separate(contrast, into = c("t1", "t2"), sep = " - ") %>% 
  filter(grepl("other", t1)) %>% 
  filter(grepl("other", t2)) %>% 
  separate(t1, into = c("covercat1", "cctrt_id1", "weayear1"), sep = " ") %>% 
  separate(t2, into = c("covercat2", "cctrt_id2", "weayear2"), sep = " ") %>% 
  filter(cctrt_id1 == cctrt_id2)





# example code ------------------------------------------------------------

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
