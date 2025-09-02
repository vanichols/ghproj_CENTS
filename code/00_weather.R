# created 5 nov 2024
# purpose: classify years (wet, hot, etc)
# notes: 

library(tidyverse)
library(CENTSdata)
library(gghighlight)
library(plotly)

rm(list = ls())

# data --------------------------------------------------------------------

w <- cents_wea
h <- cents_gdds



# 1. GDDs from planting ---------------------------------------------------

#--number of gdds accumulated from planting to sampling biomass
cents_gdds %>% 
  left_join(cents_eukey) %>% 
  select(ccest_year, pl2samp_gdd, cctrt_id) %>% 
  distinct()

# LT values ---------------------------------------------------------------

w %>% 
  summarise(avgte = mean(avgte))

w %>% 
  group_by(year) %>% 
  summarise(precip = sum(prec_mm)) %>% 
  summarise(mp = mean(precip))

# precip all year ------------------------------------------------------------------

w2 <- 
  w %>% 
  group_by(year) %>% 
  mutate(cprec_mm = cumsum(prec_mm),
         dprec_mm = cprec_mm - LTcprec_mm) %>% 
  ungroup()

w2 %>% 
  ggplot() +
  geom_line(aes(doy, dprec_mm, group = year, color = year)) +
  gghighlight(year == 2019) 


# temperature all year-------------------------------------------------------------

t3 <- 
  w %>% 
  group_by(year) %>% 
  mutate(dt_c = avgte - LTavgte,
         cdt_c = cumsum(dt_c)) %>% 
  ungroup()

t3 %>% 
  ggplot() +
  geom_line(aes(doy, cdt_c, group = year, color = year)) +
  gghighlight(year == 1992) 

ggplotly(
t3 %>% 
  ggplot() +
  geom_line(aes(doy, cdt_c, group = year, color = as.factor(year))) 
)


w_class <- 
  tibble(year = c(2018, 2019, 2020),
         precip = c("dry", "wet", "average"),
         te = c("hot", "hot", "hot"))

w_class %>% 
  write_csv("data/tidy_weaclass.csv")

# precip after harvest------------------------------------------------------------------

#--when was harvest
m <- 
  cents_cropops %>% 
  select(harvest_doy, year = harvest_year)

#--let us say 218 is harvest day


w3 <- 
  w %>% 
  filter(doy > 218) %>% 
  group_by(year) %>% 
  mutate(cprec_mm = cumsum(prec_mm),
         dprec_mm = cprec_mm - (LTcprec_mm - 311)) %>% #--to center it on 0 
  ungroup()

w3 %>% 
  ggplot() +
  geom_line(aes(doy, dprec_mm, group = year, color = year)) +
  gghighlight(year == 2018) 

summary(w3$year)

# temperature after harvest-------------------------------------------------------------

t3 <- 
  w %>% 
  group_by(year) %>% 
  mutate(dt_c = avgte - LTavgte,
         cdt_c = cumsum(dt_c)) %>% 
  ungroup()

t3 %>% 
  ggplot() +
  geom_line(aes(doy, cdt_c, group = year, color = year)) +
  gghighlight(year == 1992) 

ggplotly(
  t3 %>% 
    ggplot() +
    geom_line(aes(doy, cdt_c, group = year, color = as.factor(year))) 
)


w_class <- 
  tibble(year = c(2018, 2019, 2020),
         precip = c("dry", "wet", "average"),
         te = c("hot", "hot", "hot"))

w_class %>% 
  write_csv("data/tidy_weaclass.csv")
