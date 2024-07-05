# created 1/7/24
# purpose: use eugene's spray data to calculate PLI for each cc system
# notes: This is a rough draft, as I don't have information in the form i want it...
#       I'm excluding the insecticides and fungicides

library(tidyverse)
library(readxl)
library(lubridate)

Sys.setenv(LANG = "en")
rm(list = ls())


# raw data ----------------------------------------------------------------

draw <- 
  read_excel("data/raw/eugene_operations.xlsx", skip = 5) %>% 
  janitor::clean_names() %>% 
  mutate(op_id = 1:n())

praw <- 
  read_excel("PLI-calcs/data/by-hand_products-and-cas.xlsx", sheet = "products") %>% 
  janitor::clean_names()


p <- 
  praw %>% 
  select(product_name, ai_id, ai_name, ai_amt, ai_unit)


# clean up eugene's data --------------------------------------------------

#NOTE: Once I can change these to parcels, I need to make sure to eliminate the ones where something wasn't done to a certain subplot!!
#--in the end I'll have op_ids linked to a parcels, and op_ids linked to ai_amts and products

#--sometimes there is more than one product listed, separate

#--practice
draw %>% 
  filter(op_id == 35) %>% 
  separate(products,into = c("p1", "p2"), sep = ";") %>% 
  separate(amounts,into = c("a1", "a2"), sep = ";") %>% 
  unite(p1, a1, col = "prod1", sep = ";") %>% 
  unite(p2, a2, col = "prod2", sep = ";") %>% 
  pivot_longer(prod1:prod2) %>% 
  mutate_if(is.character, str_trim, "both") %>% 
  separate(value, into = c("product", "amount"), sep = ";") %>% 
  mutate_if(is.character, str_trim, "both") %>% 
  separate(amount, into = c("amt", "amt_unit"), sep = " ")


d1prod <- 
  draw %>% 
  mutate(date = as_date(date)) %>% 
  #--eliminate non-herbicide operations
  filter(!is.na(products)) %>% 
  separate(products,into = c("p1", "p2"), sep = ";") %>% 
  separate(amounts,into = c("a1", "a2"), sep = ";") %>% 
  unite(p1, a1, col = "prod1", sep = ";") %>% 
  unite(p2, a2, col = "prod2", sep = ";") %>% 
  pivot_longer(prod1:prod2) %>% 
  filter(value != "NA;NA") %>% 
  mutate_if(is.character, str_trim, "both") %>% 
  separate(value, into = c("product_name", "amount"), sep = ";") %>% 
  mutate_if(is.character, str_trim, "both") %>% 
  separate(amount, into = c("amt", "amt_unit"), sep = " ")

#--need to extract the active ingredient and amount sprayed, this will be messy
#--ignore the subplots for now, oh jesus this is a mess

#--to be merged with p
p

d2 <- 
  d1prod %>% 
  left_join(p) %>% 
  select(op_id, date, product_name, amt, amt_unit, ai_id, ai_name, ai_amt, ai_unit) %>% 
  mutate(amt = as.numeric(amt)) %>% 
  #--change g/kg to g/g
  mutate(ai_amt = ifelse(ai_unit == "g/kg", ai_amt/1000, ai_amt),
         ai_unit = ifelse(ai_unit == "g/kg", "g/g", ai_unit)) %>% 
  #--calculate ai applied per ha, in this case it is all g
  mutate(ai_gha = amt * ai_amt)


#--this is what will get fed into the pli calcs
d2 %>% 
  write_csv("PLI/PLI_data/tidy_ais-applied.csv")
