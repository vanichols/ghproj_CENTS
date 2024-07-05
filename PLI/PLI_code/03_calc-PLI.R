# created 4/7/24
# purpose: use r package to calculate pli
# notes: 

library(tidyverse)
library(readxl)
library(lubridate)
library(PesticideLoadIndicator)

Sys.setenv(LANG = "en")
rm(list = ls())

# practice ----------------------------------------------------------------
products_user <- products.load()

#--need to figure out how to pull data from ppdb to figure out if it is worth a purchase
substances_user <- substances.load()

indicators_user <- compute_pesticide_load_indicator(substances = substances_user,
                                                    products = products_user)



# mine --------------------------------------------------------------------

my_prods <- 
  read_csv("PLI/PLI_data/tidy_myproducts.csv")

my_prods2 <- 
  my_prods %>% 
  mutate(crop = "kale", standard.dosage = 2) %>% 
  bind_rows(my_prods)

my_subs <- read_csv("PLI/PLI_data/tidy_mysubstances.csv")

my_inds <- compute_pesticide_load_indicator(substances = my_subs,
                                                    products = my_prods)


as_tibble(my_inds)

my_inds2 <- compute_pesticide_load_indicator(substances = my_subs,
                                            products = my_prods2)


as_tibble(my_inds2) %>% 
  arrange(product, crop) %>% 
  write_csv("PLI/PLI_data/test.csv")
