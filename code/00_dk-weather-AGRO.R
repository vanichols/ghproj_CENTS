# created 8/4/2024
# purpose: learn to get weather data from DMI
# notes:


library(tidyverse)
library(lubridate)
library(scales)

rm(list = ls())



# agro climate data -------------------------------------------------------

d <- 
  read_csv("data/raw/wea/AGRO-slagelse-US.csv") %>% 
  mutate(date = dmy(date),
         doy = yday(date))

summary(d)
#--10 years, need more for long term but can write code


# wea long term -----------------------------------------------------------

#####---temperature----#####

airtempLT <- 
  d %>% 
  mutate(doy = yday(date)) %>% 
  group_by(doy) %>% 
  summarise(airtemp_avg = mean(temp))


####----cumulative precip----####

precLT <- 
  d %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  mutate(cprec = cumsum(prec)) %>% 
  group_by(doy) %>% 
  summarise(cprecLTavg = mean(cprec))


d %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  mutate(cprec = cumsum(prec)) %>% 
    ggplot(aes(doy, cprec)) + 
    geom_line(aes(color = year, group = year))


d %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  mutate(cprec = cumsum(prec)) %>%
  group_by(doy) %>% 
  mutate(cprec_max = max(cprec),
         cprec_min = min(cprec)) %>% 
  left_join(precLT) %>% 
  filter(doy < 365) %>% 
  ggplot(aes(doy, cprec)) + 
  geom_ribbon(aes(x = doy, ymin = cprec_min, ymax = cprec_max)) +
  geom_line(aes(color = as.factor(year), group = year)) + 
  geom_line(aes(x = doy, y = cprecLTavg), color = "red", size = 2)




# i love this figure! -----------------------------------------------------

d %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  mutate(cprec = cumsum(prec)) %>%
  group_by(doy) %>% 
  mutate(cprec_max = max(cprec),
         cprec_min = min(cprec)) %>% 
  left_join(precLT) %>% 
  filter(doy < 365) %>% 
  select(-(minte:prec)) %>% 
  mutate(cprec_delta = cprec - cprecLTavg) %>% 
  separate(date, into = c("xyear", "xmonth", "xday"), remove = F) %>% 
  mutate(date2 = paste("1900", xmonth, xday, sep = "-")) %>% 
  filter(!(xmonth == "02" & xday == "29")) %>% 
  mutate(date3 = ymd(date2)) %>% 
  ggplot(aes(date3, cprec_delta)) + 
  geom_line(aes(group = year), color = "gray") +
  geom_line(data = . %>% filter(year %in% c(2018, 2019)),
            aes(color = as.factor(year), group = year)) + 
  scale_x_date(labels = date_format("%e %b"))
            
