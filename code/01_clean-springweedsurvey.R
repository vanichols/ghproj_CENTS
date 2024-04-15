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
  filter(plot_id == "3_05_02" & year == 2018) %>% 
  filter(is.na(yield_DM), !is.na(reg))

huh <- 
  draw %>% 
  filter(!is.na(dicot))

#--I am not sure what these measurements are

d2 <- 
  draw %>% 
  filter(!is.na(dicot)) %>% 
  select(plot_id:date2, 
         rep = reg, 
         dicot:equar) %>% 
  pivot_longer(dicot:equar) %>% 
  mutate(value2 = as.numeric(value))

d2 %>% 
  write_csv("data/tidy/td_springweedsurvey.csv")

# what is this? -----------------------------------------------------------

d2 %>% 
  left_join(plot_key) %>% 
  ggplot(aes(plot_id, value2)) + 
  geom_col(aes(fill = name)) +
  facet_grid(cctrt_id~year, scales = "free_x")
