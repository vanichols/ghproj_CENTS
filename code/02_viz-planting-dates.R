# created 30/6/2024
# purpose: Trying new viz for cc planting dates
# notes:

library(tidyverse)
library(readxl)

rm(list = ls())

source("code/00_viz-settings.R")

# raw data ----------------------------------------------------------------

cc_key <- read_csv("data/keys/key_cctrt.csv")

cc_pl <- read_csv("data/tidy/td_cc-plant-term-dates.csv") %>% 
  mutate(cctrt_id = factor(cctrt_id, 
                           levels = c("mix_E", "mix_M", "rad_M", "rad_L", "nocc")),
         cctrt_id = fct_rev(cctrt_id))

#--sequential, I like this if I can add crop planting dates and tillage events?
cc_pl %>%
  mutate(est_year = as.factor(est_year)) %>% 
  select(till_id, cctrt_id, est_year, date, activity) %>% 
  pivot_wider(names_from = activity, values_from = date) %>%
  filter(!is.na(est_year)) %>% 
  ggplot(aes(y = cctrt_id)) + 
  geom_linerange(aes(xmin = cc_planting, xmax = cc_termination, color = cctrt_id),
                 size = 5) + 
  facet_grid(till_id~.)

#--separate years
cc_pl %>%
  mutate(est_year = as.factor(est_year)) %>% 
  select(till_id, cctrt_id, est_year, doy, activity) %>% 
  pivot_wider(names_from = activity, values_from = doy) %>%
  filter(!is.na(est_year)) %>%
  mutate(cc_termination = ifelse(cc_termination < cc_planting, 
                                 cc_termination + 365, 
                                 cc_termination)) %>% 
  ggplot(aes(y = cctrt_id)) + 
  geom_linerange(aes(xmin = cc_planting, xmax = cc_termination, color = cctrt_id),
                 size = 5) + 
  facet_grid(till_id~est_year)


