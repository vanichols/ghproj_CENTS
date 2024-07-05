# created 1/7/24
# purpose: use r package to calculate pli
# notes: using example data from Mette right now

library(tidyverse)
library(readxl)
library(lubridate)
library(PesticideLoadIndicator)

Sys.setenv(LANG = "en")
rm(list = ls())

#No fucking idea

# practice ----------------------------------------------------------------
products_user <- products.load()

products_user %>% 
  write_csv("PLI/PLI_data/pkgex_products-user.csv")

#--need to figure out how to pull data from ppdb to figure out if it is worth a purchase
substances_user <- substances.load()

substances_user %>% 
  write_csv("PLI/PLI_data/pkgex_substances-user.csv")

indicators_user <- compute_pesticide_load_indicator(substances = substances_user,
                                                    products = products_user)

indicators_user %>% 
  write_csv("PLI/PLI_data/pkgex_results.csv")


# get my substances into their form --------------------------------------------------------------------


#--to get all that info you have to go to https://sitem.herts.ac.uk/aeru/ppdb/ and get each active manually
#--mette gave me a sheet I can use for now, if there is overlap

mette_sub <- 
  read_excel("PLI/PLI_data/mette_substances-england.xlsx") %>% 
  mutate_if(is.character, str_to_lower)

#--note there are some repeated substances, and they have different values. This is wrong, but I must ignore it and just use one of them if there are multiple matches
mette_sub %>% pull(substance)


#--fix it up so it matches
d_raw <- read_csv("PLI/PLI_data/tidy_ais-applied.csv")

d_sub <- 
  d_raw %>% 
  mutate_if(is.character, str_to_lower) %>% 
  #--get concentrations in kg per kg (the units they want)
  mutate(concentration = case_when(
    ai_unit == "g/l" ~ ai_amt / 1000,
    ai_unit == "g/g" ~ ai_amt,
    TRUE ~ 99999
  )) %>% 
  filter(concentration != 99999) %>% 
  select(product = product_name, substance = ai_name, concentration) %>% 
  distinct()

my_subs <- 
  d_sub %>% 
  pull(substance) %>% 
  unique()

d_mysubstances <- 
  mette_sub %>% 
  select(-product, -concentration) %>% 
  filter(substance %in% my_subs) %>%  
  arrange(substance) %>% 
  distinct() %>% 
  #--we are good, no repeats
  left_join(d_sub) %>% 
  select(substance, product, concentration, everything())

#--ready to go into the function!
d_mysubstances %>% 
  write_csv("PLI/PLI_data/tidy_mysubstances.csv")


# products ----------------------------------------------------------------

products_user

mette_pro <- 
  read_excel("PLI/PLI_data/mette_products-england.xlsx") %>% 
  mutate_if(is.character, str_to_lower)

#--need amount applied in L/ha
d_pro <- 
  d_raw %>% 
  mutate_if(is.character, str_to_lower) %>% 
  #--get concentrations in kg per kg (the units they want)
  mutate(amount.applied = case_when(
    amt_unit == "l/ha" ~ amt,
    amt_unit == "g/ha" ~ amt/1000,
    TRUE ~ 99999
  )) %>% 
  filter(amount.applied != 99999) %>% 
  select(product = product_name, amount.applied) %>% 
  distinct()

#--any overlap? no
d_pro %>% 
  left_join(mette_pro %>% 
  select(-amount.applied))

#--hand made one based on compute_risk_score
r <- 
  read_excel("PLI/PLI_data/pkgex_risk-table-handmade-from-code.xlsx") %>% 
  janitor::clean_names()

##--split it up to see if either phrases have matchews
r_risk <- 
  r %>% 
  select(code = r_number, risk_score_inherent, load_i_per_kg_product) %>% 
  filter(!is.na(code)) 
  
h_risk <- 
  r %>% 
  select(code = h_number, risk_score_inherent, load_i_per_kg_product) %>% 
  filter(!is.na(code))

r_new <- 
  r_risk %>% 
  bind_rows(h_risk)

d_r <- read_excel("PLI/PLI_data/by-hand_risk-phrases.xlsx", skip = 5)

#--glyphosate has a risk score of 0
d_rnew <- 
  d_r %>% 
  left_join(r_new)

#--let's try two standard doses
products_user

d_risk <- 
  d_rnew %>% 
  group_by(product) %>% 
  summarise(sum.risk.score = sum(risk_score_inherent, na.rm = T)) %>% 
  left_join(d_pro) %>% 
  mutate(reference.sum.risk.scores = 350,
         formula = 1.5, 
         standard.dosage = 1,
         crop = "eggplant") %>% 
  select(product, crop, sum.risk.score, reference.sum.risk.scores, formula, amount.applied, standard.dosage)

d_risk %>% 
  write_csv("PLI/PLI_data/tidy_myproducts.csv")
