# created 8/4/2024
# purpose: calculate GDDs from AGRO weather
# notes:


library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(lubridate)

rm(list = ls())


# weather -----------------------------------------------------------------

wea <- read_csv("data/tidy/td_wea.csv")


# planting dates ----------------------------------------------------------

pl <-  read_csv("data/tidy/td_ccpl.csv")

# growing degree days -----------------------------------------------------

# for each cctrt_id
cclist <- pl %>% pull(cctrt_id) %>% unique()

for (i in 1:length(cclist))
  
  i <- 1
  tmp.cc <- cclist[i]
  
  pl.tmp <- pl %>% filter(cctrt_id == tmp.cc)
  
  wea %>%
    mutate(year = year(date)) %>% 
    filter(year %in% c("2018", "2019")) %>% 
    left_join(pl.tmp) %>% 
    filter(doy >= pldoy) %>% 
    mutate(minte2 = ifelse(minte < 0, 0, minte),
           )
