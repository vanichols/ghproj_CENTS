#--make summary of which species were sampled in which msmts
#--created Jan 30 2025

library(tidyverse)
library(CENTSdata)


#--just categories
cents_fallbio %>% 
  select(date2, dm_type) %>% 
  distinct()

fc <- 
  cents_fallpctcover %>% 
  select(eppo_code) %>% 
  distinct() %>% 
  mutate(measurement = "Fall ground cover") %>% 
  left_join(cents_species) %>% 
  select(measurement, latin_name) %>% 
  filter(!is.na(latin_name)) %>% 
  mutate(latin_name = str_to_sentence(latin_name)) %>% 
  arrange(latin_name)

cents_spweedcount
