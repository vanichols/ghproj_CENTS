# created 5 nov 2024
# purpose: do stats on vegetation cover
# notes: 3 subreps make up a single eu

# 4 cats of cover: soil cover, weed cover, covercrop, volunteer

# results: model is not fitting well...
# issues: there is one treatment combination that had 0s in all instances
#         soil cover


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

w <- cents_wea
eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_fallpctcover)
w <- read_csv("data/tidy_weaclass.csv")

#--data separated by species
d_sp <- 
  eu %>% 
  left_join(y) %>% 
  mutate(year = year(date2)) %>% 
  left_join(w) %>% 
  mutate(weayear = paste(precip, te, sep  = "-"),
         cover_frac = cover_pct / 100)

#--data separated by cateogry (covercrop, soil, other)
d_cat <-
  d_sp %>% 
  group_by(block_id, plot_id, subplot_id, till_id, straw_id, 
           cctrt_id, subrep, weayear, cover_cat) %>% 
  summarise(cover_pct = sum(cover_pct),
            cover_frac = sum(cover_frac)) 

#--does it add to 100? Yes, I have the exp unit correctly defined
d_cat.tst <-
  d_cat %>% 
  group_by(block_id, plot_id, subplot_id, till_id, straw_id, 
           cctrt_id, subrep, weayear) %>% 
  summarise(tot = sum(cover_pct))

d_cat.tst %>% 
  ggplot(aes(tot)) +
  geom_histogram()


# viz ---------------------------------------------------------------------

d_cat 

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

#--it is actally only one combination where there are 0s everywhere
d_cat |> 
  group_by(till_id, straw_id, cctrt_id, weayear, cover_cat) |> 
  summarise(mn = mean(cover_pct)) |> 
  arrange(mn)


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

#--correlation of raw data, cover crop and other
d_cat %>% 
  filter(cover_cat != "soil") %>% 
  dplyr::select(cover_cat, cctrt_id, weayear, cover_frac) %>% 
  pivot_wider(names_from = cover_cat, values_from = cover_frac) %>% 
  ggplot(aes(covercrop, other)) +
  geom_point(aes(color = weayear))

#--correlation of raw data
d_cat %>% 
  filter(cover_cat != "other") %>% 
  dplyr::select(cover_cat, cctrt_id, weayear, cover_frac) %>% 
  pivot_wider(names_from = cover_cat, values_from = cover_frac) %>% 
  ggplot(aes(covercrop, soil)) +
  geom_point(aes(color = weayear))

library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)

d_corr <- 
  d_cat %>% 
  ungroup() %>% 
  dplyr::select(subplot_id, weayear, subrep, cover_cat, cover_pct) %>% 
  pivot_wider(names_from = cover_cat, values_from = cover_pct) %>% 
  dplyr::select(covercrop, other, soil)

res <- 
  cor(d_corr)

corrplot(res)
chart.Correlation(d_corr)


d_sp |> 
  filter(cover_cat2 == "volunteer") |>
  group_by(eu_id, date2) |> 
  summarise(cover_pct = mean(cover_pct)) |> 
  pull(cover_pct) |> 
  summary()


d_sp |> 
  group_by(cover_cat2) |> 
  summarise(cover_pct = mean(cover_pct))


# model -------------------------------------------------------------------

# For the 4-way proportions for cover, you could actually analyse this a little differently to what we talked about
# as well. You could pivot your data into long-format that has a column for the proportions, and another column
# for the Cover category (Soil, Weeds, Volunteers and CC). If you then analyse the proportions with a model like
# this:
  # model <- glmmTMB(proportion ~ CoverCategory * Tillage * CC * Straw * Year +
  #                    (1|Block) + (1|CC:Tillage:Straw) + (1|Tillage:Straw) + (1|Straw),
  #                  family=binomial(link=”logit”), data=DATA)
# You get an estimate of how different the proportions of each cover category are, and you are checking whether
# that is dependent on tillage, CC, straw or year e􀆯ects.

#--I am super confused which family (etc.) to use
#--normally use a beta for continuous proportions, but 
#-- here, bc there may be some values of 0, we use ordbeta
#--the logit link maps the linear predictor to the data scale (which is 0-1)
#--there may be subtle differences between choices, logit and probit are the most common
m_1 <- glmmTMB(cover_frac ~ till_id * cctrt_id * straw_id * weayear +
                   (1|block_id/straw_id/till_id/cctrt_id),
                 family=ordbeta(link = "logit"), 
              data=d_cat |> filter(cover_cat == "covercrop"))

Anova(m_1)
m_1_simres <- simulateResiduals(m_1)
plot(m_1_simres)
m1 <- m_1


#--the model fits well, do I need to account for more 0s in the nocc treatment?
#--the ordbeta is 'already' accounting for the zeros
#--you COULD model it separately for each category
#--it doesn't seem to be necessary bsaed on the plots
#--you WOULD use this ziformula to do that
m_1zinfc <- update(m_1, ziformula = ~cctrt_id)
m_1z_simres <- simulateResiduals(m_1zinfc)
plot(m_1z_simres)


#--ignore the pvalues here - it is only valid for nested models!!
anova(m_1, m_1zinfc)
#--the extra complication is not worth it!

car::Anova(m_1)

#--as a demo, let's group by year (huge effect probably) and look at cover crop impacts

emmeans(m_1, specs = ~ cctrt_id|weayear, type = "response")


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

tidy(emmeans(m1, specs = ~ cover_cat, type = "response"))
(emmeans(m1, specs = pairwise ~ cover_cat:cctrt_id, type = "response"))

#--correlation of modelled data
em1_est %>% 
  filter(cover_cat != "soil") %>% 
  dplyr::select(cover_cat, cctrt_id, weayear, prob) %>% 
  pivot_wider(names_from = cover_cat, values_from = prob) %>% 
  ggplot(aes(covercrop, other)) +
  geom_point()


#--correlation of raw data
d_cat %>% 
  filter(cover_cat != "soil") %>% 
  dplyr::select(cover_cat, cctrt_id, weayear, cover_frac) %>% 
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
  facet_grid(.~till_id)# +
  #scale_y_continuous(limits = c(0, 4.7))
