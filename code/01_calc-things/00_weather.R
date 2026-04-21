#--heat units accumulation visualiztion
library(tidyverse)
library(scales)
library(CENTSdata)
library(lubridate)

i <- 0

df <- NULL

for (i in 0:110){
  df.tmp <- 
    cents_wea %>% 
    filter(doy <320) %>% 
    filter(doy > 210+i) %>% 
    mutate(heat_units = avgte - 5,
           heat_units = ifelse(heat_units < 0, 0, heat_units)) %>% 
    group_by(year) %>% 
    summarise(accum_heat_units = sum(heat_units)) %>% 
    mutate(doy_start = 201+i) #%>% 
    #group_by(doy_start) %>% 
    #summarise(accum_heat_units_avg = mean(accum_heat_units))
  
  df <- 
    df %>% 
    bind_rows(df.tmp)
  
}

#--get dates attached to the doys
df1 <- 
  df %>% 
  rename(doy = doy_start) %>% 
  left_join(cents_wea %>%
              filter(year == 1990) %>%
              select(date2, doy)) %>% 
  #--make a dummy date
  mutate(date3 = as.Date(paste0("2012-", format(date2, "%m-%d")))) 


df_mean <-
  df1 %>%
  group_by(doy, date3) %>%
  summarise(accum_heat_units = mean(accum_heat_units))

ggplot() +
  geom_point(data = df1,
             aes(x = date3, 
                 y = accum_heat_units)) +
  geom_line(data = df_mean,
             aes(date3, accum_heat_units),
             color = "red",
            linewidth = 3) +
  scale_x_date(labels = label_date_short(format = "%b-%d"))
  
  
