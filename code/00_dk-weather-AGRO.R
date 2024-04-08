# created 8/4/2024
# purpose: learn to get weather data from DMI
# notes:


library(tidyverse)


rm(list = ls())



# agro climate data -------------------------------------------------------


read_csv("data/raw/wea/AGRO-slagelse-US.csv") %>% 
  
