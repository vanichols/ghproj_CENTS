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
  filter(!is.na(reg), is.na(dicot)) 

draw %>% 
  filter(!is.na(lamss)) ->a

#--is senss:lamss = weedcov?
# senss is ragwort I think, senecio sp.
# lamss is deadnettle? Lamium?

# 
d2 <- 
  draw %>% 
  filter(!is.na(reg), is.na(dicot)) %>% 
  select(plot_id:date2, 
         rep = reg, 
         soil:weedcov) 

#--are the weeds assessed the same in each year?
#--Yes
d2 %>% 
  filter(year == 2018) %>% 
  pivot_longer(senss:lamss) %>% 
  select(year, name) %>% 
  distinct() %>% 
  mutate(cheat = "A") %>% 
  left_join(d2 %>% 
              filter(year == 2019) %>% 
              pivot_longer(senss:lamss) %>% 
              select(year2 = year, name) %>% 
              distinct() %>% 
              mutate(cheat = "A"))
  
#--yes
d2 %>% 
  left_join(
    d2 %>% 
      pivot_longer(senss:lamss) %>% 
      group_by(plot_id, year, date2, rep) %>% 
      summarise(weedcov2 = sum(value, na.rm = T))
    
  ) %>% 
  ggplot(aes(weedcov, weedcov2)) + 
  geom_point(aes(color = as.factor(year)))


d2

# tidy it -----------------------------------------------------------------

d1 <- 
  draw %>% 
  filter(!is.na(reg), is.na(dicot)) %>% 
  select(plot_id:date2, 
         rep = reg, 
         soil:radish, weedcov) 


#--all measurements very, very close to 100%. Wow. 
d1 %>% 
  pivot_longer(soil:weedcov) %>% 
  group_by(plot_id, year, date, rep) %>% 
  summarise(tot = sum(value, na.rm = F)) %>% 
  ggplot(aes(tot)) + 
  geom_histogram()
  

d3 <- 
  d1 %>% 
  pivot_longer(soil:weedcov) %>% 
  rename(cover_type = name, 
         cover_pct = value) 

d3 %>% 
  write_csv("data/tidy/td_weedcover.csv")


summary(d3)
  
d3 %>% 
  filter(year == 2018) %>% 
  select(cover_type) %>% 
  distinct()

# look at it --------------------------------------------------------------

d3 %>% 
  ggplot(aes(plot_id, cover_pct)) + 
  geom_col(aes(fill = cover_type)) + 
  facet_grid(.~year)

d3 %>% 
  left_join(plot_key) %>% 
  mutate(cover_type2 = ifelse(cover_type %in% c("clover", "lolpe", "radish"), "cc", cover_type)) %>% 
  ggplot(aes(plot_id, cover_pct)) + 
  geom_col(aes(fill = cover_type2)) + 
  facet_wrap(~year + cctrt_id, scales = "free", ncol = 5) + 
  scale_fill_manual(values = c("green4", "black", "purple", "red")) + 
  coord_flip()

