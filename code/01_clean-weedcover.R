# created 26/3/2024
# purpose: clean/separate Bo's CENTs data
# notes:


library(tidyverse)
library(plotly)
library(ggplotlyExtra)

rm(list = ls())



# raw data ----------------------------------------------------------------

draw <- read_csv("data/raw/rd_gina-simplified.csv")

plot_key <- read_csv("data/keys/key_plot.csv")

cc_key<- read_csv("data/keys/key_cctrt.csv")


# percent weed coverage ---------------------------------------------

tst18 <- 
  draw %>% 
  filter(plot_id == "3_05_02" & year == 2018)

#--the weeds!!!! keep the weed coverage, duh
#  this is wrong
d2 <- 
  draw %>% 
  filter(is.na(yield_DM), !is.na(reg)) %>% 
  select(plot_id:date2, 
         rep = reg), 
         soil, volunteer, clover, lolpe, radish) %>%
  pivot_longer(soil:radish) 


# numbers don't add up to 100 ---------------------------------------------

d3 <- 
  d2  %>% 
  group_by(plot_id, year, date, date2, rep) %>% 
  mutate(tot = sum(value, na.rm = T)) %>% 
  filter(tot > 0) %>% 
  unite(plot_id, date2, col = "obs_id", remove = F) 

d3.tmp <- 
  d3 %>% 
  select(obs_id, plot_id, year, date2, rep, tot) %>% 
  distinct() %>% 
  left_join(plot_key) %>% 
  unite(plot_id, date2, col = "obs_id", remove = F) 

d3.tmp %>%
  ggplot(aes(obs_id, tot)) + 
    geom_point(aes(color = cctrt_id, shape = till_id), show.legend = F) + 
  facet_grid(.~year)

ggplotly(
)

#--720 observations
#--100 instances where total is less than 95

d3 %>% 
  select(obs_id, plot_id, year, date, date2, rep, tot) %>% 
  distinct() %>% 
  filter(tot <95)

#--example
d3 %>% 
  filter(obs_id == "3_53_04_2019-11-01") %>% 
  filter(rep == 3) %>% 
  left_join(plot_key) %>% 
  left_join(cc_key)


#--what would you do?
#--option is to rescale, make all values total to 100
#--or leave as is, compare soil values to other soil values
