# created 8/4/2024
# purpose: process weather from AGRO weather station
# notes:


library(tidyverse)
library(lubridate)
library(scales)

rm(list = ls())

# get data from
# https://agro-web11t.uni.au.dk/klimadb/

# agro climate data -------------------------------------------------------

d1 <- 
  read_csv("data/raw/wea/1990-2013 AGRO-slagelse-US.csv") %>% 
  rename(prec = prec08) %>% 
  mutate(date = ymd(date),
         doy = yday(date),
         station = 613500) 

tst <- 
  d1 %>% 
  mutate(minte2 = as.numeric(minte))

tst2 <- 
  tst %>% 
  filter(is.na(minte2))

#--are there more NAs in a given year?
#--hmmm maybe this data isn't worth it
#--could take averages of every other year and fill them in I guess
tst2 %>% 
  mutate(year = year(date)) %>% 
  ggplot(aes(year)) + 
  geom_histogram()

d1a <- 
  d1 %>% 
  mutate_if(is.character, as.numeric) 

d1b <- 
  d1a %>%
  group_by(doy) %>% 
  mutate(minte_avg = mean(minte, na.rm = T),
         maxte_avg = mean(maxte, na.rm = T),
         temp_avg = mean(temp, na.rm = T),
         prec_avg = mean(prec, na.rm = T)) %>% 
  pivot_longer(minte:prec) %>% 
  mutate(value = case_when(
    (is.na(value) & name == "minte") ~ minte_avg,
    (is.na(value) & name == "maxte") ~ maxte_avg,
    (is.na(value) & name == "temp") ~ temp_avg,
    (is.na(value) & name == "prec") ~ prec_avg,
    TRUE ~ value
  )) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  select(station, date, doy, minte:prec)

#--it worked...STOPPED
d1b %>% 
  filter(is.na(minte))

d2 <- 
  read_csv("data/raw/wea/AGRO-slagelse-US.csv") %>% 
  mutate(date = dmy(date),
         doy = yday(date))

summary(d2)
#--10 years, need more for long term but can write code

d <- 
  d1b %>% 
  bind_rows(d2)

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
            aes(color = as.factor(year), group = year), linewidth = 1.2) + 
  scale_x_date(labels = date_format("%e %b")) +
  labs(x = NULL,
       y = "Deviation from long-term average (mm)")
            
