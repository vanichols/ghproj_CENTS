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


# dry matter --------------------------------------------------------------

#-- I think it is g/m2, need to confirm with Bo

tst18 <- 
  draw %>% 
  filter(plot_id == "3_05_03" & year == 2019)


d1 <- 
  draw %>% 
  select(plot_id, year, date, date2, dm_type = frac, dm_gm2 = DM) %>% 
  filter(!is.na(dm_type))

summary(d1 %>% 
          mutate_if(is.character, as.factor))


d2 <- 
  d1 %>% 
  #--correct volunteer spelling
  mutate(dm_type = ifelse(dm_type == "voluntee", "volunteer", dm_type)) 

summary(d2 %>% 
          mutate_if(is.character, as.factor))

d2 %>% 
  write_csv("data/tidy/td_fallbio.csv")

# look at to check --------------------------------------------------------

summary(d1)

#--just two sampling dates, I think
d1 %>% 
  mutate(dm_type2 = ifelse(dm_type %in% c("grass_cl", "radish"), "cover crop", dm_type)) %>% 
  ggplot(aes(plot_id, dm_gm2)) + 
  geom_col(aes(fill = dm_type2)) + 
  facet_grid(.~year) + 
  scale_fill_manual(values = c("green4", "gray80", "gray20"))

