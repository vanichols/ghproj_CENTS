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


# precip ------------------------------------------------------------------

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


# temperature -------------------------------------------------------------

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
