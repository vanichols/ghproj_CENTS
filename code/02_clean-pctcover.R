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


# percent coverage ---------------------------------------------

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

# 1. keep species separate -----------------------------------------------------------------

#--keep individual weeds for the full dataset

d1 <- 
  draw %>% 
  filter(!is.na(reg), is.na(dicot)) %>% 
  select(plot_id:date2, 
         rep = reg, 
         soil:lamss)


#--all measurements very, very close to 100%. Wow. 
d1 %>% 
  pivot_longer(soil:lamss) %>% 
  group_by(plot_id, year, date, rep) %>% 
  summarise(tot = sum(value, na.rm = T)) %>% 
  ggplot(aes(tot)) + 
  geom_histogram()
  

d2 <- 
  d1 %>% 
  pivot_longer(soil:lamss) %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>% 
  rename(cover_type = name, 
         cover_pct = value) 
 
d2 %>% 
  write_csv("data/tidy/td_pctcover.csv")


summary(d2)

d2 %>% 
  filter(year == 2018) %>% 
  select(cover_type) %>% 
  distinct()

d2 %>% 
  ggplot(aes(plot_id, cover_pct)) + 
  geom_col(aes(fill = cover_type)) + 
  facet_grid(.~year)


# 2. make generic catch crop/weed cover categories ---------------------------


d1simp <- 
  draw %>% 
  filter(!is.na(reg), is.na(dicot)) %>% 
  select(plot_id:date2, 
         rep = reg, 
         soil:radish, weedcov) %>% 
  pivot_longer(soil:weedcov) %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>% 
  rename(cover_type = name, 
         cover_pct = value) 


d2simp <- 
  d1simp %>% 
  mutate(cover_type2 = case_when(
  (cover_type %in% c("clover", "lolpe", "radish")) ~ "covercrop", 
  cover_type == "weedcov" ~ "weeds", 
  TRUE ~ cover_type)) %>% 
  group_by(plot_id, year, date2, rep, cover_type2) %>% 
  summarise(cover_pct = sum(cover_pct, na.rm = T))

d2simp %>% 
  rename(date = date2) %>% 
  write_csv("data/tidy/td_pctcover-simple.csv")

d2simp %>% 
  left_join(plot_key) %>% 
  ggplot(aes(plot_id, cover_pct)) + 
  geom_col(aes(fill = cover_type2)) + 
  facet_wrap(~year + cctrt_id + straw, scales = "free", ncol = 6) + 
  scale_fill_manual(values = c("green4", "black", "purple", "red")) + 
  #coord_flip() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom")

d2simp %>% 
  left_join(plot_key) %>% 
  ggplot(aes(plot_id, cover_pct)) + 
  geom_col(aes(fill = cover_type2)) + 
  facet_wrap(~year + cctrt_id, scales = "free", ncol = 5) + 
  scale_fill_manual(values = c("green4", "black", "purple", "red")) + 
  #coord_flip() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom")
