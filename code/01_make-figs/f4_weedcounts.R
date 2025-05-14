# created 11 may 2025
# purpose: visualize spring weed counts
# notes: 3 subreps

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
y <- as_tibble(cents_spweedcount)
w <- read_csv("data/tidy_weaclass.csv")

draw <- 
  y %>% 
  mutate(year = year(date2),
         yearF = paste("Y", year)) %>% 
  left_join(eu) %>% 
  left_join(w) %>% 
  mutate(weayear = paste(year, precip, sep = "-"),
         weed_type2 = ifelse(weed_type %in% c("dicot", "monocot"), "A", "P"))


# viz ---------------------------------------------------------------------

draw

#--there are lots of zeros!
draw %>% 
  ggplot(aes(count)) +
  geom_histogram()

draw %>% 
  ggplot(aes(count)) +
  geom_histogram(aes(fill = weed_type2))

#--definitely over-dispersed
draw %>% 
  summarise(ave = mean(count),
            var = sd(count)^2)

#--need to sum up the sub-reps
d <- 
  draw %>% 
  group_by(year, yearF, weed_type, weed_type2, 
           block_id, plot_id, subplot_id, 
           straw_id, till_id, cctrt_id, 
           weayear) %>% 
  summarise(count = sum(count)) %>% 
  ungroup()

d2 <-
  d %>% 
  group_by(year, yearF, block_id, plot_id, subplot_id, straw_id, till_id, cctrt_id,
           weed_type2) %>% 
  summarize(cat_tot = sum(count)) %>% 
  group_by(year, yearF, block_id, plot_id, subplot_id, straw_id, till_id, cctrt_id) %>% 
  mutate(tot = sum(cat_tot),
         cat_pct = cat_tot/tot*100) 

glimpse(d2)

d2 %>%
  filter(weed_type2 == "P") %>% 
  group_by(year, yearF, straw_id, till_id, cctrt_id) %>% 
  summarise(tot = mean(tot),
            cat_pct = mean(cat_pct)) %>% 
  ggplot(aes(year, tot)) +
  geom_point(aes(size = cat_pct, colour = till_id)) +
  geom_line(aes(linetype = straw_id, color = till_id)) +
  facet_grid(till_id~cctrt_id)

d2 %>%
  filter(weed_type2 == "P") %>% 
  group_by(year, yearF, straw_id, till_id, cctrt_id) %>% 
  summarise(tot = mean(tot),
            cat_pct = mean(cat_pct)) %>% 
  ggplot(aes(year, tot)) +
  geom_point(aes(size = cat_pct, colour = cctrt_id)) +
  geom_line(aes(linetype = straw_id, color = cctrt_id)) +
  facet_grid(.~till_id)


d2 %>%
  filter(weed_type2 == "P") %>% 
  group_by(year, yearF, straw_id, till_id, cctrt_id) %>% 
  summarise(tot = mean(tot),
            cat_tot = mean(cat_tot),
            cat_pct = mean(cat_pct)) %>% 
  ggplot() +
  geom_point(aes(x = year, y = tot, size = tot), color = "black") +
  geom_point(aes(x = year, y = tot, size = cat_tot), color = "orange") +
  geom_line(aes(x = year, y = tot, linetype = straw_id)) +
  facet_grid(till_id~cctrt_id)


d2 %>%
  filter(weed_type2 == "P") %>% 
  group_by(year, yearF, straw_id, till_id, cctrt_id) %>% 
  summarise(tot = mean(tot),
            cat_tot = mean(cat_tot),
            cat_pct = mean(cat_pct)) %>% 
  ggplot() +
  geom_point(aes(x = yearF, y = straw_id, size = tot), color = "black") +
  geom_point(aes(x = yearF, y = straw_id, size = cat_tot), color = "orange") +
  facet_grid(till_id~cctrt_id)


#--removing straw reduced weeds in surface and inversion tillages
#--less weeds in notill, but much higher percentage of them are perennial
#--less annual weeds in notill, but equal or greater perennial weeds
#--mixE in the notill had more perennials and more annuals compared to other cctrts in notilll
d2 %>%
  filter(weed_type2 == "P") %>% 
  group_by(straw_id, till_id, cctrt_id) %>% 
  summarise(tot = mean(tot),
            cat_tot = mean(cat_tot),
            cat_pct = mean(cat_pct),
            tot_pct = 100) %>% 
  ggplot() +
  geom_point(aes(x = straw_id, y = till_id, size = tot), color = "black") +
  geom_point(aes(x = straw_id, y = till_id, size = cat_tot), color = "orange") +
  scale_radius(range = c(1, 6)) +
  facet_grid(.~cctrt_id)

