# created 26/3/2024
# purpose: clean/separate Bo's CENTs data
# notes:


library(tidyverse)

rm(list = ls())

theme_set(theme_bw())


# raw data ----------------------------------------------------------------

draw <- read_excel("data/raw/dkstats-small-grains.xlsx", skip = 2)

d1 <- 
  draw %>% 
  #janitor::clean_names() %>% 
  select(crop:ncol(.)) %>% 
  pivot_longer(2:ncol(.)) %>% 
  mutate(year = as.numeric(name))

d1 %>% 
  #--what is a hkga?
  mutate(yield_kgha = value*100,
         yield_Mgha = yield_kgha/1000) %>% 
  group_by(crop) %>% 
  mutate(avg_yield = mean(yield_Mgha)) %>% 
  ggplot(aes(year, yield_Mgha)) + 
  geom_line(aes(color = crop), size = 1) + 
  geom_hline(aes(yintercept = avg_yield, color = crop), size = 2)

