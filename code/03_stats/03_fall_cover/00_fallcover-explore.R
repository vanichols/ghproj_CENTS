# created 5 nov 2024
# notes: 3 subreps make up a single eu
# 4 cats of cover: soil cover, weed cover, covercrop, volunteer

# issues: there is one treatment combination that had 0s in all instances
#         soil cover


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

#--does it add to 100? Yes, I have the exp unit correctly defined
d_cat.tst <-
  d_cat %>% 
  group_by(block_id, plot_id, subplot_id, till_id, straw_id, 
           cctrt_id, subrep, weayear) %>% 
  summarise(tot = sum(cover_pct))

d_cat.tst %>% 
  ggplot(aes(tot)) +
  geom_histogram()


# data exploration ---------------------------------------------------------------------

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


#--correlation of raw data, cover crop and other
#--this it to be expected, it is a proportion so things must be related
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

#--data summaries
d_sp |> 
  filter(cover_cat2 == "volunteer") |>
  group_by(eu_id, date2) |> 
  summarise(cover_pct = mean(cover_pct)) |> 
  pull(cover_pct) |> 
  summary()

d_sp |> 
  filter(cover_cat2 == "covercrop") |>
  group_by(eu_id, date2) |> 
  summarise(cover_pct = mean(cover_pct)) |> 
  pull(cover_pct) |> 
  summary()

d_sp |> 
  filter(cover_cat2 == "weed") |>
  group_by(eu_id, date2) |> 
  summarise(cover_pct = mean(cover_pct)) |> 
  pull(cover_pct) |> 
  summary()

d_sp |> 
  group_by(cover_cat2) |> 
  summarise(cover_pct = mean(cover_pct))


