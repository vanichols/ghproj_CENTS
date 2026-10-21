#--created 21 oct 2026
#--use 'utilities' in each category
#--have different weights depending on value system


library(tidyverse)
library(CENTSdata)
library(readxl)

rm(list = ls())

source("code/00_color-palettes.R")


# 1. Multicriteria values -------------------------------------------------

d1 <- read_csv("data/tidy_multicriteria-values.csv")

d1 |> 
  select(name) |> 
  distinct()

# 2. typology weights -----------------------------------------------------

d2 <- 
  read_excel("data/byhand_typology-weights2.xlsx") |> 
  select(-total) |> 
  pivot_longer(yield:minitox) |> 
  rename(weight = value)


# 3. combine --------------------------------------------------------------

d3 <- 
  d1 %>% 
  left_join(d2, relationship = "many-to-many") |> 
  mutate(wgtd_value = value * weight/100) 



# 4. calc new total values ------------------------------------------------

d4 <- 
  d3 %>% 
  group_by(farmer_type, cctrt_id, till_id, straw_id) %>% 
  summarise(valuetot = sum(wgtd_value)) 



# 5. look at it -----------------------------------------------------------

d4 |> 
  mutate(cropsys = paste(cctrt_id, till_id, straw_id)) |> 
  ggplot(aes(reorder(cropsys, valuetot), valuetot, fill = farmer_type)) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~farmer_type)

d5 <- 
  d4 |> 
  mutate(cropsys = paste(cctrt_id, till_id, straw_id)) |>
  arrange(farmer_type, -valuetot) |> 
  group_by(farmer_type) |>
  mutate(rank = 1:n())

d5 |> 
  ggplot(aes(farmer_type, rank, color = cropsys, group = cropsys)) +
  geom_line()

d5 <- 
  d4 %>% 
  mutate(cropsys = paste(cctrt_id, till_id, straw_id)) 
  group_by(farmer_type) %>% 
  slice_max(n = 3, order_by = valuetot)

d5 %>% 
  ggplot(aes(farmer_type, valuetot, fill = cropsys)) +
  geom_col(position = "dodge")


# 6. write -------------------------------------------------------------------

d4 |> 
  write_csv("data/tidy_typology-totvalues.csv")


