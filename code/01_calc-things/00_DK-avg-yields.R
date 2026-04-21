#--stats from Denmark on crop yields
#--note: are these dry? or assume a % moisture?

library(tidyverse)


# DK yields ---------------------------------------------------------------

dk <- 
  readxl::read_excel("data/dkstats-small-grain-yields-over-time.xlsx", 
                     skip = 2) %>% 
  select(6:ncol(.)) %>% 
  slice(1:6) %>% 
  janitor::clean_names() %>%
  mutate(x2009 = as.numeric(x2009)) %>% 
  pivot_longer(2:ncol(.), names_to = "year", values_to = "yield") %>% 
  mutate(year = str_remove(year, "x"),
         year = as.numeric(year)) 

dk %>% 
  group_by(crop) %>% 
  summarise(yield = mean(yield, na.rm = T)/10)

#--barley
d1 <- 
  dk %>% 
  filter((crop == "Spring barley" & year == 2018)) %>% 
  group_by(crop, year) %>% 
  summarise(yield = mean(yield)/10)

#--oats
d2 <- 
  dk %>% 
  filter((crop == "Oats, mixed grains and other grains" & year == 2019)) %>% 
  group_by(crop, year) %>% 
  summarise(yield = mean(yield)/10)

#--faba
d3 <- 
  dk %>% 
  filter((crop == "faba beans" & year == 2020)) %>% 
  group_by(crop, year) %>% 
  summarise(yield = mean(yield)/10)

d <- 
  d1 %>% 
  bind_rows(d2) %>% 
  bind_rows(d3)

d %>% 
  write_csv("data/tidy_dk-avg-yields.csv")
