# created 27/5/2024
# purpose: separate data into ind data sets, clean if necessary
# notes:
# 1. spring weed counts following year
# 2. fall plant biomass
# 3. fall plant cover
# 4. crop yields (2018 - spring barely, 2019 - oat, 2020 - faba bean)

#--there are essentially two crop years of data
# 2018 spring and fall, plus 2019 spring before planting of next crop
# 2019 spring and fall, plus 2020 spring before planting of next crop
# 2020 crop yields are also included

#--I added an 'experimental year'
# 2019 spring msmts (weed counts) are connected to previous year's treatments
# exp_year 1_fall, 1_subsp, 2_fall, 2_subsp
# crop yields will be harder, no experimental year for them I think


library(tidyverse)
library(readxl)

Sys.setenv(LANG = "en")
rm(list = ls())


# raw data ----------------------------------------------------------------

draw <- read_excel("data/raw/Data_Gina.xls")

pkey <- 
  read_csv("data/keys/key_plot.csv") %>% 
  mutate(parc = as.character(parc))

#--clean up the column names so they make sense in my world
draw2 <- 
  draw %>% 
  select(-block) %>% 
  mutate(parc = as.character(parc),
         date2 = lubridate::dmy(date),
         year = lubridate::year(date2)) %>% 
  left_join(pkey) %>% 
  select(plot_id, yield_DM, date2, year, date:weedcov) 


# 1. spring weed counts ------------------------------------------

#--three subreps in each plot

#--for some reason, it reads in the dicot/monocot etc. as T/F, not as counts
draw %>% 
  filter(!is.na(dicot))

#--I manually filtered the raw dataset in Excel to save as it's own file

d1raw <- 
  read_excel("data/raw/Data_Gina-filtered-springweedcounts.xlsx") 

d1 <- 
  d1raw %>% 
  select(-block) %>% 
  mutate(parc = as.character(parc),
       date2 = lubridate::dmy(date),
       year = lubridate::year(date2)) %>% 
  left_join(pkey)  %>% 
  mutate(exp_year = ifelse(year == 2019, "1_subsp", "2_subsp")) %>% 
  select(plot_id, year, exp_year, date2, subrep = reg, dicot, monocot, cirar, equar)  

d1 %>% 
  write_csv("data/raw/rd_springweedcounts.csv")


# 2. fall biomass ---------------------------------------------------------

#NOTE: are there subreps? I don't believe so
# no separation of weed types, just categorically 'weeds'
# NOTE: in 2018 it is barley volunteers, in 2019 it is oat volunteers (no data)

d2 <- 
  draw2 %>% 
  mutate(exp_year = ifelse(year == 2018, "1_fall", "2_fall")) %>% 
  filter(!is.na(frac)) %>% 
  select(plot_id, year, exp_year, date2, dm_type = frac, dm_gm2 = DM) %>% 
  #--correct volunteer spelling
  mutate(dm_type = ifelse(dm_type == "voluntee", "volunteer", dm_type))  %>% 
  #--make NA values explicit, it isn't clear which are 0s and which are missing
  pivot_wider(names_from = dm_type, values_from = dm_gm2) %>%
  pivot_longer(grass_cl:volunteer) %>%
  arrange(plot_id, year, date2) %>%
  rename(dm_type = name, dm_gm2 = value)

summary(d2)

d2 %>% 
  write_csv("data/raw/rd_fallbio.csv")


#--NOTE: 2019 had no volunteer biomass data
d2 %>%
  filter(!is.na(dm_gm2)) %>% 
  ggplot(aes(dm_type)) + 
  geom_histogram(stat = "count") +
  labs(x = NULL,
       y = "number of observations",
       title = "No biomass for volunteers in 2019") +
  facet_grid(.~year)

#--just two sampling dates, I think
d2 %>% 
  mutate(dm_type2 = ifelse(dm_type %in% c("grass_cl", "radish"), "cover crop", dm_type)) %>% 
  ggplot(aes(plot_id, dm_gm2)) + 
  geom_col(aes(fill = dm_type2)) + 
  facet_grid(.~year) + 
  scale_fill_manual(values = c("green4", "gray80", "gray20"))


# 3. fall pct cover -------------------------------------------------------

#--there are 3 subreps w/in each plot

d3 <- 
  draw2 %>% 
  filter(!is.na(reg), is.na(dicot)) %>% 
  mutate(exp_year = ifelse(year == 2018, "1_fall", "2_fall")) %>% 
  select(plot_id:date2, 
         year,
         exp_year,
         subrep = reg, 
         soil:lamss,
         -yield_DM) %>% 
  pivot_longer(soil:lamss) %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>% 
  rename(cover_type = name, 
         cover_pct = value) %>% 
  #--make generic cover types
  mutate(cover_type2 = case_when(
  (cover_type %in% c("clover", "lolpe", "radish")) ~ "covercrop", 
  cover_type %in% c("radish", "senss", "verss", "capbp", "paprh", "cirss") ~ "weed", 
  TRUE ~ cover_type)) %>% 
  select(plot_id:cover_type, cover_type2, cover_pct, everything())

summary(d3)

d3 %>% 
  write_csv("data/raw/rd_fallcover.csv")


# 4. crop yields ----------------------------------------------------------
#--this one is a bit difficult, year wise, and I'm not sure 

d4 <-
  draw2 %>% 
  filter(!is.na(yield_DM)) %>% 
  select(plot_id:year, 
         yield_dry_gm2 = yield_DM, 
         yield_15p_tonha = yield15) %>% 
  mutate(crop = case_when(
    year == 2018 ~ "spring barley",
    year == 2019 ~ "oat",
    year == 2020 ~ "faba bean")) %>% 
  select(plot_id, year, crop, date2, everything()) 

d4 %>% 
  write_csv("data/raw/rd_cropyields.csv")


