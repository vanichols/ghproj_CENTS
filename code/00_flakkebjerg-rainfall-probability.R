library(tidyverse)
library(ggthemes)

theme_set(theme_clean())

d <- read_csv("data/tidy/td_wea.csv")

#--probably need more than 10 years of data...
d %>% 
#  filter(doy < 50) %>% 
  mutate(
    doy = yday(date),
    rainedYN = ifelse(prec_mm > 2, 1, 0)) %>% 
  group_by(doy) %>% 
  summarise(
    tot = n(),
    rain_prob = sum(rainedYN/tot)) %>% 
  ggplot(aes(doy, rain_prob)) + 
  geom_rect(xmin = 200, xmax = 233, 
            ymin = 0, ymax = 1,
            fill = "red",
            alpha = 0.5) +
  geom_col(width = 0.4) 

