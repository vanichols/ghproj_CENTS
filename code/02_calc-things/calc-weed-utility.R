# created 6 dec
# assign utility to weed communities
# based on schatke et al 2024

library(tidyverse)
library(readxl)
library(CENTSdata)

cents_spweedcount

# make sure I understand the scaling they used ----------------------------

d <- read_excel("data/schatke-weed-utilities.xlsx", sheet = "weed-util", skip = 5)

dsc <- read_excel("data/schatke-weed-utilities.xlsx", sheet = "weed-scaled-util", skip = 5)


d_test <- 
  d %>% 
  select(-other) %>% 
  pivot_longer(S1:D6) %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(name) %>% 
  mutate(max = max(value),
         min = min(value)) %>% 
  mutate(scvalue_calc = ( (value - min)/(max - min)))


#--something is wrong....
dsc %>% 
  select(-other) %>% 
  pivot_longer(S1:D6) %>% 
  mutate(scvalue = as.numeric(value)) %>% 
  select(-value) %>% 
  left_join(d_test) %>% 
  mutate(tst = scvalue_calc - scvalue) %>% 
  ggplot(aes(name, tst)) + 
  geom_point(aes(color = latin), size = 5)
  
