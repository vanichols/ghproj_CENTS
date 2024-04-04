# created 26/3/2024
# purpose: clean/separate Bo's CENTs data
# notes:


library(tidyverse)
library(readxl)

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


# 1. reassign tillage treatments ---------------------------------------------

d1 <- 
  draw %>% 
  mutate(till_id = case_when(
    till == 1 ~ "high",
    till == 2 ~ "med",
    till == 3 ~ "none")) %>% 
  select(till_id, everything())


# 2. Make block non-numeric -----------------------------------------------

d2 <- 
  d1 %>% 
  mutate(block = paste0("B", block))



# 3. straw assignment -----------------------------------------------------

d3 <- 
  d2 %>% 
  mutate(straw_id = ifelse(rota == "R4", "retained", "removed"))


# 4. extract year from date -----------------------------------------------

d4 <- 
  d3 %>% 
  mutate(
    date2 = lubridate::dmy(date),
    year = lubridate::year(date2))


# 5. reassign cover crop, create key --------------------------------------

d5 <- 
  d4 %>% 
  mutate(cctrt_id = case_when(
    covercrop == "lolcl1" ~ "mix_E",
    covercrop == "lolcl2" ~ "mix_M",
    covercrop == "radish1" ~ "rad_M",
    covercrop == "radish2" ~ "rad_L",
    covercrop == "noCC" ~ "nocc",
    TRUE ~ "XXXX")) %>% 
  select(cctrt_id, everything())


cc_key <- 
  d5 %>%
  select(cctrt_id, covercrop, estab, species) %>%
  distinct() %>%
  rename(B_covercrop = covercrop,
         B_estab = estab,
         B_species = species) %>%
  mutate(
    estab_desc = case_when(
      cctrt_id == "mix_E" ~ "at crop sowing",
      cctrt_id == "mix_M" ~ "2 weeks before crop harvest",
      cctrt_id == "rad_M" ~ "2 weeks before crop harvest",
      cctrt_id == "rad_L" ~ "after crop harvest",
      TRUE ~ "no cover crop established"
    ),
    species_desc = case_when(
      B_species == "lol_tri" ~ "Lolium X and Trifolium pratense (ryegrass and red clover)",
      B_species == "raphanu" ~ "Raphanus sativus (forage radish)",
      TRUE ~ "none"
    )
  ) %>% 
  mutate(cctrt_label = case_when(
           cctrt_id == "mix_E" ~ "Gr/cl at sowing",
         cctrt_id == "mix_M" ~ "Gr/cl pre-harvest",
         cctrt_id == "rad_M" ~ "Rad pre-harvest",
         cctrt_id == "rad_L" ~ "Rad post-harvest",
         TRUE ~ "Control")
  )

cc_key %>% 
  write_csv("data/tidy/td_cctrt-key-long.csv")
  

cc_key %>% 
  select(cctrt_id, cctrt_label) %>% 
  write_csv("data/tidy/td_cctrt-key.csv")



# 6. make plot key --------------------------------------------------------

# plots should keep same treatments each year
# 2 rot trts, 3 tillage trts, 5 cc trts, 4 reps
2*3*5*4

plot_key <- 
  d5 %>% 
  select(plot_id = parc, block, straw_id, till_id, cctrt_id) %>% 
  distinct()

plot_key %>% 
  

# 6. slim down ------------------------------------------------------------

d6 <-
  d5 %>% 
  select(year, block, parc, till_id, cctrt_id, straw_id, date2, everything(),
         -till, -rota, -covercrop, -estab, -species)

# 7. yields ---------------------------------------------------------------

d7 <-
  d6 %>% 
  filter(!is.na(yield_DM)) %>% 
  select(year, block, parc, till_id, cctrt_id, straw_id, date2, 
         yieldDM_gm2 = yield_DM, 
         yield15_tonha = yield15)

d7 %>% 
  write_csv("data/tidy/td_yield.csv")
