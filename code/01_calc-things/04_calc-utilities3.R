#--create data to make star chart
#--created 31 oct 2025
#--average over years etc., then scale as last step
#--6 criteria currently
#--27 jan 2026 - reran with new PLI values using updated faba bean value and Agil matching - didn't really check
#--22 april 2026 - update to use emmeans when applicable

library(tidyverse)
library(CENTSdata)
library(readxl)

rm(list = ls())


# 1. species ecovalue GOOD--------------------------------------------------------------

d1a <- 
  read_csv("data/tidy_communityvalue.csv")

#--scale from 0-1, with 1 being maximum value observed and 0 being 0
d1b <- 
  d1a %>% 
  mutate(potval_sc = potval/max(potval))

#--we have a range from 0-1, good
d1b %>% 
  ggplot(aes(potval_sc)) +
  geom_histogram()

#--full dataset
d1c <- 
  d1b %>% 
  left_join(cents_eukey)

#--mean value for each cc, till, straw
d1d <- 
  d1c %>% 
  group_by(cctrt_id, till_id, straw_id) %>% 
  summarise(potval = mean(potval))

d1d %>% 
  ggplot(aes(potval))+
  geom_histogram()

#--now scale
d1e <- 
  d1d %>% 
  ungroup() %>% 
  mutate(potval_sc = potval/max(potval))

d1e %>% 
  ggplot(aes(potval_sc))+
  geom_histogram()


d1 <- 
  d1e %>% 
  mutate(criteria = "fallvegvalue") %>% 
  select(-potval, value = potval_sc)

# 2. biomass GOOD-----------------------------------------

#--get total biomass per eu in each year
d2a <- read_csv("data/stats/figs_emmeans/starburst-emmeans-fallbio.csv")

#--simplify dataset
d2b <- 
  d2a %>% 
  select(cctrt_id, 
         till_id, 
         straw_id, 
         dm_gm2 = emmean)

d2b %>% 
  ggplot(aes(dm_gm2))+
  geom_histogram()

#--now scale
d2c <- 
  d2b %>% 
  ungroup() %>% 
  mutate(dm_gm2_sc = dm_gm2/max(dm_gm2))

d2c %>% 
  ggplot(aes(dm_gm2_sc))+
  geom_histogram()

d2 <- 
  d2c %>% 
  mutate(criteria = "fallbio") %>% 
  select(-dm_gm2, value = dm_gm2_sc)


# 3. grain yields GOOD--------------------------------------------------

#--get total grain yields over all years for each eu
d3a <- 
  read_csv("data/stats/figs_emmeans/starburst-emmeans-yield.csv")
  
#--simplify dataset
d3b <- 
  d3a %>% 
  select(cctrt_id, till_id, straw_id, yield = emmean)

d3b %>% 
  ggplot(aes(yield))+
  geom_histogram()

#--now scale
d3c <- 
  d3b %>% 
  ungroup() %>% 
  mutate(yield_sc = yield/max(yield))

d3c %>% 
  ggplot(aes(yield_sc))+
  geom_histogram()

d3 <- 
  d3c %>% 
  mutate(criteria = "yield") %>% 
  select(-yield, value = yield_sc)

# 4. PLI BAD, switch scale------------------------------------------------------------------

d4a <- 
  read_csv("data/tidy_pesticide-load-by-eu.csv")

#--get total load over years for each eu
d4b <- 
  d4a %>% 
  group_by(eu_id) %>% 
  summarise(load = sum(load_ha, na.rm = T)) %>% 
  ungroup()

#--mean value for each cc, till, straw
d4c <- 
  d4b %>% 
  left_join(cents_eukey) %>% 
  group_by(cctrt_id, till_id, straw_id) %>% 
  summarise(load = mean(load))

d4c %>% 
  ggplot(aes(load))+
  geom_histogram()

#--now scale
d4d <- 
  d4c %>% 
  ungroup() %>% 
  mutate(load = load/max(load))

d4d %>% 
  ggplot(aes(load))+
  geom_histogram()

#--now switch scales, highest load is actually worst
d4e <- 
  d4d %>% 
  mutate(load_rev = 1 - load)

d4e %>% 
  ggplot(aes(load_rev))+
  geom_histogram()

d4 <- 
  d4e %>% 
  mutate(criteria = "minitox") %>% 
  select(-load, value = load_rev)

# 5. spring perennial weeds BAD-----------------------------------------------------------

#--total cirar and equar counted (perenn weeds)

d5a <- read_csv("data/stats/figs_emmeans/starburst-emmeans-pweeds.csv")

#--simplify
d5b <- 
  d5a %>% 
  select(cctrt_id, till_id, straw_id, pweed = response)

d5b %>% 
  ggplot(aes(pweed))+
  geom_histogram()

#--now scale
d5c <- 
  d5b %>% 
  ungroup() %>% 
  mutate(pweed = pweed/max(pweed))

d5c %>% 
  ggplot(aes(pweed))+
  geom_histogram()

#--now switch scales, highest load is actually worst
d5e <- 
  d5c %>% 
  mutate(pweed_rev = 1 - pweed)

d5e %>% 
  ggplot(aes(pweed_rev))+
  geom_histogram()

d5 <- 
  d5e %>% 
  mutate(criteria = "perennweeds") %>% 
  select(-pweed, value = pweed_rev)


# 6. soil protection ------------------------------------------------------

#--get total coverage (all cats) per subrep 

d6a <- read_csv("data/stats/figs_emmeans/starburst-emmeans-soilcover.csv")

#--simplify
d6b <- 
  d6a %>% 
  select(cctrt_id, till_id, straw_id, pctcov = response)

d6b %>% 
  ggplot(aes(pctcov))+
  geom_histogram()

#--now scale
d6c <- 
  d6b %>% 
  ungroup() %>% 
  mutate(pctcov_sc = pctcov/max(pctcov))

d6c %>% 
  ggplot(aes(pctcov_sc))+
  geom_histogram()

d6 <- 
  d6c %>% 
  mutate(criteria = "pctcov") %>% 
  select(-pctcov, value = pctcov_sc)



# 7. combine -------------------------------------------------------

d7 <- 
  d1 %>%
  bind_rows(d2) %>% 
  bind_rows(d3) %>% 
  bind_rows(d4) %>% 
  bind_rows(d5) %>% 
  bind_rows(d6) %>% 
  rename(name = criteria)

d7


# write -------------------------------------------------------------------

d7 %>% 
  write_csv("data/tidy_multicriteria-values.csv")

