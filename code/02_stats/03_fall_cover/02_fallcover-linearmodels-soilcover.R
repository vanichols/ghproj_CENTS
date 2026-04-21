# created 5 nov 2024
# purpose: do stats on vegetation cover
# notes: 3 subreps make up a single eu

# 4 cats of cover: soil cover, weed cover, covercrop, volunteer
# 9 april 2026, started cleaning up for publishing

library(CENTSdata)

library(tidyverse)
library(writexl)

library(broom)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(car)

rm(list = ls())

# data --------------------------------------------------------------------

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

#--generalized categories (soil, other)
d_cat2 <- 
  d_cat |> 
  mutate(cover_cat2 = ifelse(cover_cat == "soil", "soil", "cover")) |> 
  group_by(block_id, plot_id, subplot_id, till_id, straw_id, 
           cctrt_id, subrep, weayear, cover_cat2) |> 
  summarise(cover_pct = sum(cover_pct),
            cover_frac = sum(cover_frac),
            cover_frac = ifelse(cover_frac > 1, 0.99999, cover_frac)) 

# model on soil cover-------------------------------------------------------------------

#--run same model as cc but on soil fraction
m2 <- glmmTMB(cover_frac ~ till_id * cctrt_id * straw_id * weayear +
                (1|block_id/straw_id/till_id/cctrt_id),
              family=ordbeta(link = "logit"), 
              data=d_cat2 |> filter(cover_cat2 != "soil"))

#--lots of interactions
Anova(m2)
m2_simres <- simulateResiduals(m2)
plot(m2_simres)

#--the model fits fine

#--write final model anova table
tidy(car::Anova(m2)) |> 
  write_xlsx("data/stats/supp_tables/fallcover-soilcover-anova.xlsx")

# sunburst chart ----------------------------------------------------------

#--write the esimates averaged over years for the sunburst chart
emmeans(m2, specs = ~ cctrt_id|till_id|straw_id, type = "response") |> 
  as_tibble() |> 
  mutate(perf_cat = "soil coverage") |> 
  write_csv("data/stats/figs_emmeans/starburst-emmeans-soilcover.csv")

# emmeans soilcover -----------------------------------------------------------------

#--the things to look at
tidy(car::Anova(m2)) |> 
  filter(p.value < 0.05)

#--mostly cover crop and year, their interaction
#--look at most sig diffs on absolute scale (3% change in soil cover not that interesting)

#--coverage is fairly stable over weayears when averaged over treatments
emmeans(m2, specs = ~ weayear, type = "response") |> 
  as_tibble() |>
  write_xlsx("data/stats/supp_tables/fallcover-soilcover-wea-estimates.xlsx")


# cover crop stuff --------------------------------------------------------

#--I want to compare each cctrt x weayear combibnation to each other

#--cctrt x weayear
#--you use a *, that is how you get estimates across all of them
em0 <- 
  emmeans(m2, specs = ~ weayear*cctrt_id, type = "response") 

#--cctrt x weayear
em1a <- 
  emmeans(m2, specs = ~ weayear|cctrt_id, type = "response") 

#--should be the same results, sliced differently
em1b <- 
  emmeans(m2, specs = ~ cctrt_id|weayear, type = "response") 


cld0 <- multcomp::cld(em0, Letters = letters)  |> 
  as.data.frame()

cld1a <- multcomp::cld(em1a, Letters = LETTERS)  |> 
  as.data.frame()
(cld1b <- multcomp::cld(em1b, Letters = letters) |> 
    as.data.frame() |> 
    dplyr::select(weayear, cctrt_id, .group) |> 
    rename(.group2 = .group))

cld1 <- left_join(cld1a, cld1b)

#--to get a feeling
em2 <- 
  emmeans(m2, specs = ~ cctrt_id, type = "response") 

#--these letters make sense
cld1 <- 
  multcomp::cld(em1, by = 'weayear', Letters = letters) |> 
  as_tibble() |> 
  mutate(.group = ifelse(weayear == "dry-hot", str_to_upper(.group), .group),
         .group = str_trim(.group)) 

cld1 |> 
  as_tibble() |> 
  ggplot(aes(cctrt_id, response, color = cctrt_id)) +
  geom_point() +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_text(aes(y = response - 0.2, label = .group)) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_grid(.~weayear)

#--I want letters for each cctrt x wea combination...
#--this doesn't seem to work
cld2 <- 
  multcomp::cld(em0, Letters = letters) |> 
  as_tibble() |> 
  mutate(.group = str_trim(.group)) 

#--look at it...
#--mixE stayed constant over years, as did mixM
#--no cc soil coverage varied by year the most of any treatment
#--so including a cover crop provides stability in soil coverage
cld2 |> 
  as_tibble() |> 
  ggplot(aes(cctrt_id, response, color = cctrt_id)) +
  geom_point() +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_text(aes(y = response - 0.2, label = .group)) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_grid(.~weayear)



# tillage -----------------------------------------------------------------

em3 <- 
  emmeans(m2, specs = ~ weayear*till_id, type = "response") 

em3

emmeans(m2, specs = ~ till_id, type = "response") 


multcomp::cld(em3, Letters = letters) |> 
  as_tibble() |> 
  mutate(.group = str_trim(.group)) |> 
  ggplot(aes(till_id, response, color = till_id)) +
  geom_point() +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_text(aes(y = response - 0.2, label = .group)) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_grid(.~weayear)

em4 <- 
  emmeans(m2, specs = ~ cctrt_id*till_id, type = "response") 



multcomp::cld(em4, Letters = letters) |> 
  as_tibble() |> 
  mutate(.group = str_trim(.group)) |> 
  ggplot(aes(cctrt_id, response, color = cctrt_id)) +
  geom_point() +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_text(aes(y = response - 0.2, label = .group)) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_grid(.~till_id)

em4 |> 
  as_tibble() |> 
  filter(cctrt_id == "mix_E")

est_list = list('mixE inv' = c(1, rep(0, 14)), 
                'mixE others' = c(rep(0, 5), 0.5, rep(0, 4), 0.5, rep(0, 4)))

contrast(regrid(em4), est_list, infer = c(T, F))

em4 |> 
  as_tibble() |> 
  write_xlsx("data/stats/supp_tables/fallcover-soilcover-cctill-estimates.xlsx")

pairs(emmeans(m2, specs = ~ till_id|cctrt_id, type = "response") ) |> 
  as_tibble() |> 
  write_xlsx("data/stats/supp_tables/fallcover-soilcover-cctill-pairs.xlsx")



# weather -----------------------------------------------------------------


#--dry hot year (2018), biggset diff was btwn mixE and mixM
emmeans(m2, specs = ~ cctrt_id|weayear, type = "response") |> 
  as_tibble() |> 
  filter(weayear == "dry-hot")
#--8% coverage diff
0.82-0.74


#--wet hot year (201), biggset diff was btwn mixE and nocc
emmeans(m2, specs = ~ cctrt_id|weayear, type = "response") |> 
  as_tibble() |> 
  filter(weayear == "wet-hot")
#--8% coverage diff
0.84-0.61

emmeans(m2, specs = ~ cctrt_id|weayear, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(weayear, response, color = cctrt_id)) +
  geom_point() +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_grid(.~cctrt_id)

#--straw interactions with weayear, are they crossover or amplifying?
#--RES: diff in one year, not the other
#--removing straw meant more soil exposure in one of the years
emmeans(m2, specs = ~ straw_id|weayear, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(straw_id, response, color = straw_id)) +
  geom_point() +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  facet_grid(.~weayear)

#--so there is a difference in wet-hot year, but not other year
#--straw removed had higher soil exposure in wet-hot year
(emmeans(m2, specs = pairwise ~ straw_id:weayear, type = "response"))$contrasts |> 
  as_tibble() |> 
  separate(contrast, into = c("t1", "t2"), sep = "/") %>%
  mutate(t1 = str_trim(t1),
         t2 = str_trim(t2)) |> 
  separate(t1, into = c("cctrt1", "w1"), sep = " ") %>% 
  separate(t2, into = c("cctrt2", "w2"), sep = " ") |> 
  filter(w1 == w2) |> 
  arrange(w1, p.value) |> 
  mutate(psimp = round(p.value, 3))



#--this is really only like 3%
(emmeans(m2, specs = pairwise ~ straw_id:weayear))$contrasts |> 
  as_tibble() |> 
  separate(contrast, into = c("t1", "t2"), sep = " - ") %>%
  mutate(t1 = str_trim(t1),
         t2 = str_trim(t2)) |> 
  separate(t1, into = c("cctrt1", "w1"), sep = " ") %>% 
  separate(t2, into = c("cctrt2", "w2"), sep = " ") |> 
  filter(w1 == w2) |> 
  arrange(w1, p.value) |> 
  mutate(psimp = round(p.value, 3))

#--look at raw values
emmeans(m2, specs = ~ straw_id|weayear, type = "response") |> 
  as_tibble()
#--so a difference of 3%, not meaningful


#--nocc and radishes had much higher soil exposure in the wet/hot year
#--mixes were not impacted
emmeans(m2, specs = ~ cctrt_id|weayear, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(cctrt_id, response, color = cctrt_id)) +
  geom_point() +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  facet_grid(.~weayear)

#--are there sig diffs w/in the dry-hot year?
(emmeans(m2, specs = pairwise ~ cctrt_id:weayear))$contrasts |> 
  as_tibble() |> 
  separate(contrast, into = c("t1", "t2"), sep = " - ") %>% 
  separate(t1, into = c("cctrt1", "w1"), sep = " ") %>% 
  separate(t2, into = c("cctrt2", "w2"), sep = " ") |> 
  filter(w1 == w2) |> 
  arrange(w1, p.value) |> 
  mutate(psimp = round(p.value, 3))

