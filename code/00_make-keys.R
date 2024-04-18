# created 26/3/2024
# purpose: make data keys to simplify data manipulation
# notes:


library(tidyverse)
library(readxl)

Sys.setenv(LANG = "en")
rm(list = ls())



# raw data ----------------------------------------------------------------

draw <- read_excel("data/raw/Data_Gina.xls")

draw %>% 
  arrange(parc)

# make keys ---------------------------------------------------------------

#--5 cc treatments (radish early, radish late, mix early, mix late, no cc)
#--3 tillage treatments (1=aggresive, 2=moderate, 4=zero)
#--two 'rotations', but they are actually straw removal treatments (R3 = straw removal, R4 = straw remains)
#--4 replicates
#--120 experimental units

#--3 years of observations?

# each parcel should have all the info about it
# a single parcel may have been sampled multiple times


# get rid of observed variables -------------------------------------------

d0 <- 
  draw %>% 
  select(parc, block, till, rota, covercrop, estab, species, date) %>% 
  distinct()

# 1. tillage key ---------------------------------------------

d1 <- 
  d0 %>% 
  mutate(till_id = case_when(
    till == 1 ~ "high",
    till == 2 ~ "med",
    till == 4 ~ "notill"),
    till_desc = case_when(
      till == 1 ~ "moldboard plowing",
      till == 2 ~ "8-10 cm depth tillage",
      till == 4 ~ "direct seeding since exp establishment")
  )

till_key <- 
  d1 %>% 
  select(till_id, till_desc, Bo_till = till) %>% 
  distinct() %>% 
  arrange(-Bo_till) %>% 
  mutate(till_intensity = c(1, 2, 3))

till_key %>% 
  write_csv("data/keys/key_till.csv")



# 2. cover crop key -------------------------------------------------------

cc_key <- 
  d1 %>% 
  select(covercrop, estab, species) %>% 
  distinct() %>% 
  mutate(
    cctrt_id = case_when(
    covercrop == "lolcl1" ~ "mix_E",
    covercrop == "lolcl2" ~ "mix_M",
    covercrop == "radish1" ~ "rad_M",
    covercrop == "radish2" ~ "rad_L",
    covercrop == "noCC" ~ "nocc",
    TRUE ~ "XXXX")) %>% 
  rename(Bo_covercrop = covercrop,
         Bo_estab = estab,
         Bo_species = species) %>%
  mutate(
    estab_desc = case_when(
      cctrt_id == "mix_E" ~ "at crop sowing",
      cctrt_id == "mix_M" ~ "2 weeks before crop harvest",
      cctrt_id == "rad_M" ~ "2 weeks before crop harvest",
      cctrt_id == "rad_L" ~ "after crop harvest",
      TRUE ~ "no cover crop established"
    ),
    species_desc = case_when(
      Bo_species == "lol_tri" ~ "Lolium X and Trifolium pratense (ryegrass and red clover)",
      Bo_species == "raphanu" ~ "Raphanus sativus (forage radish)",
      TRUE ~ "none"
    )
  ) %>% 
  mutate(cctrt_desc = case_when(
    cctrt_id == "mix_E" ~ "Gr/cl at sowing",
    cctrt_id == "mix_M" ~ "Gr/cl pre-harvest",
    cctrt_id == "rad_M" ~ "Rad pre-harvest",
    cctrt_id == "rad_L" ~ "Rad post-harvest",
    TRUE ~ "Control")
  )

cc_key %>% 
  select(cctrt_id, species_desc, cctrt_desc, everything()) %>% 
  write_csv("data/keys/key_cctrt.csv")



# 2. Parcel key -----------------------------------------------

plot_key <- 
  d0 %>% 
  rename(Bo_till = till, Bo_covercrop = covercrop, Bo_estab = estab, Bo_species = species) %>% 
  left_join(till_key) %>%
  left_join(cc_key) %>% 
  #--change block to character
  mutate(block = paste0("B", block)) %>% 
#--assign straw as rotation 
  mutate(straw = ifelse(rota == "R4", "retained", "removed")) %>% 
  select(parc, block, straw, till_id, cctrt_id) %>% 
  distinct() %>% 
  #--separate parcel so it is a character
mutate(
  parc = as.character(parc),
  parc_id1 = str_sub(parc, 1, 1),
  parc_id2 = str_sub(parc, 2, 3),
  parc_id3 = str_sub(parc, 4, 5),
  plot_id = paste(parc_id1, parc_id2, parc_id3, sep = "_")) %>% 
  select(plot_id, parc, block, straw, till_id, cctrt_id)

plot_key %>% 
  write_csv("data/keys/key_plot.csv")



# 3. make raw data easier to deal with ---------------------------------------

d3 <- 
  draw %>% 
  select(-block) %>% 
  mutate(parc = as.character(parc),
         date2 = lubridate::dmy(date),
         year = lubridate::year(date2)) %>% 
  left_join(plot_key) %>% 
  select(plot_id, yield_DM, date2, year, date:weedcov) 

d3 %>% 
  select(plot_id, year, date, date2, everything()) %>% 
  write_csv("data/raw/rd_gina-simplified.csv")
