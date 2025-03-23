# created 23 march
# make a table for supp material

library(tidyverse)
library(openxlsx2)

d1 <-  
  read_csv("data/tidy_spvalue.csv")

d1 %>% 
  mutate_if(is.numeric, round, 2) %>%
  mutate(eppo_code = str_to_upper(eppo_code),
         latin_name = str_to_sentence(latin_name)) %>% 
  write_xlsx("data/tables/table_species-values.xlsx")
