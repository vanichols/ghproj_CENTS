# created 5 nov 2024
# purpose: do stats on vegetation cover
# notes: 3 subreps make up a single eu

# 4 cats of cover: soil cover, weed cover, covercrop, volunteer
# 9 april 2026, started cleaning up for publishing
# simon question - how to get the raw differences in proportions from emmeans?

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
  write_xlsx("data/stats/supp_tables/fallcover-anova.xlsx")


# emmeans soilcover -----------------------------------------------------------------

#--write the esimates averaged over years for the sunburst chart
emmeans(m2, specs = ~ cctrt_id|till_id|straw_id, type = "response") |> 
  as_tibble() |> 
  mutate(perf_cat = "soil coverage") |> 
  write_csv("data/stats/figs_emmeans/starburst-emmeans-soilcover.csv")

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

#--this difference doesn't really match what I 'see', which is like 3%
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

emmeans(m2, specs = ~ straw_id|weayear, type = "response") |> 
  as_tibble()

#--so a difference of 3%, not meaningful, but the pairs says 13% change, meaning 13% of the value
#--how can I get the raw difference?


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

#--get letters within a year

#--nocc, radL and radM

(emmeans(m2, specs = pairwise ~ cctrt_id:weayear))$contrasts |> 
  as_tibble() |> 
  separate(contrast, into = c("t1", "t2"), sep = " - ") %>% 
  separate(t1, into = c("cctrt1", "w1"), sep = " ") %>% 
  separate(t2, into = c("cctrt2", "w2"), sep = " ") |> 
  filter(w1 == w2) |> 
  arrange(w1, p.value) 


|> 
  write_xlsx("data/stats/emmeans/emmeans-fallcover-soil-cctrt-by-year.xlsx")



#--straw - only a difference in wet-hot year
emmeans(m2, specs = ~ straw_id|weayear, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(weayear, response, color = straw_id)) +
  geom_point() +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL))




em1_pairs <- emmeans(m1, specs = pairwise ~ cctrt_id:weayear, type = "response")
em1_pairs2 <- emmeans(m1, specs = pairwise ~ cctrt_id:weayear)

em1_est <- em1_pairs$contrasts |> as_tibble()

#--mostly different from nocc
em1_est %>% 
  ggplot(aes(reorder(contrast, odds.ratio), odds.ratio)) +
  geom_point() +
  coord_flip()

em1_res <- 
  em1_pairs2$contrasts |> 
  as_tibble() |> 
  separate(contrast, into = c("t1", "t2"), sep = " - ") %>% 
  separate(t1, into = c("cctrt1", "w1"), sep = " ") %>% 
  separate(t2, into = c("cctrt2", "w2"), sep = " ") |> 
  filter(cctrt1 == cctrt2)



emmeans(m2, specs = ~ straw_id|weayear, type = "response")

#--nothing is interacting with tillage or straw removal, consistent w/perception
#--the effect of cctrt depends on the weayear
#--some cctrts are more reslient to weather conditions than others

em1_pairs <- emmeans(m1, specs = pairwise ~ cctrt_id:weayear, type = "response")
em1_pairs2 <- emmeans(m1, specs = pairwise ~ cctrt_id:weayear)

em1_est <- em1_pairs$contrasts |> as_tibble()

#--mostly different from nocc
em1_est %>% 
  ggplot(aes(reorder(contrast, odds.ratio), odds.ratio)) +
  geom_point() +
  coord_flip()

em1_res <- 
  em1_pairs2$contrasts |> 
  as_tibble() |> 
  separate(contrast, into = c("t1", "t2"), sep = " - ") %>% 
  separate(t1, into = c("cctrt1", "w1"), sep = " ") %>% 
  separate(t2, into = c("cctrt2", "w2"), sep = " ") 

em1_res %>% 
  filter(cctrt1 == cctrt2)

em1_res %>% 
  filter(w1 == w2) |> 
  arrange(w1, p.value)




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
