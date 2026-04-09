# created 15 oct 2024
#--explore yields and 

library(tidyverse)
library(CENTSdata)

rm(list = ls())


# DK yields ---------------------------------------------------------------

readxl::read_excel("data/dkstats-small-grain-yields-over-time.xlsx", 
                   skip = 2) %>% 
  select(6:ncol(.)) %>% 
  slice(1:6) %>% 
  janitor::clean_names() %>%
  mutate(x2009 = as.numeric(x2009)) %>% 
  pivot_longer(2:ncol(.), names_to = "year", values_to = "yield") %>% 
  mutate(year = str_remove(year, "x"),
         year = as.numeric(year)) %>% 
  filter(year > 2017) %>% 
  group_by(crop) %>% 
  summarise(yield = mean(yield)/10)

dk_y <- 
  readxl::read_excel("data/dkstats-small-grain-yields-over-time.xlsx", 
                   skip = 2) %>% 
  select(6:ncol(.)) %>% 
  slice(1:6) %>% 
  janitor::clean_names() %>%
  mutate(x2009 = as.numeric(x2009)) %>% 
  pivot_longer(2:ncol(.), names_to = "year", values_to = "yield") %>% 
  mutate(year = str_remove(year, "x"),
         year = as.numeric(year)) 

dk_y %>% 
  group_by(crop) %>% 
  summarise(yield = mean(yield, na.rm = T)/10)

#--barley
dk_y %>% 
  filter((crop == "Spring barley" & year == 2018)) %>% 
  summarise(yield = mean(yield)/10)

#--oats
dk_y %>% 
  filter((crop == "Oats, mixed grains and other grains" & year == 2019)) %>% 
  summarise(yield = mean(yield)/10)

#--faba
dk_y %>% 
  filter((crop == "faba beans" & year == 2020)) %>% 
  summarise(yield = mean(yield)/10)

