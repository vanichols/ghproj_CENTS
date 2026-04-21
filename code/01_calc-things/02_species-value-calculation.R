# created 6 dec
# assign weed disservices 
# based on yvoz
# 13 june 2025 - update values to use 'organ' level
# 11 aug 2025 - cleaned up a little to get marco feedback

theme_set(theme_bw())

#--run if you don't have this package
#devtools::install_github("vanichols/CENTSdata")

library(tidyverse)
library(readxl)
library(CENTSdata)

#--make sure package works
cents_species
cents_fallpctcover


# 0. number of species ----------------------------------------------------

d0 <- 
  cents_fallpctcover %>% 
  filter(cover_pct != 0) %>% 
  filter(eppo_code != "soil") %>% 
  mutate(year = year(date2)) %>% 
  select(year, eu_id, eppo_code) %>% 
  distinct() %>% 
  group_by(eu_id, year) %>% 
  summarise(nu_sp = n())

d0 %>% 
  write_csv("data/tidy_nuspecies.csv")

d0 %>% 
  left_join(cents_eukey) %>% 
  group_by(cctrt_id) %>% 
  summarise(nu_sp = mean(nu_sp))

# 1. prioritize -----------------------------------------------------------

#--which weeds appear in a significant amount?
cents_fallpctcover %>%
  ggplot(aes(eppo_code, cover_pct)) +
  geom_col()

d1 <- 
  cents_fallpctcover %>%
  group_by(eppo_code) %>% 
  summarise(cover_pct = sum(cover_pct)) %>% 
  arrange(-cover_pct)

d1
#--we only have 16 things to assign values to (soil gets a zero no matter what, so really 15)

#--yvoz et a. 2021
#--organ level
yvoz_raw <- 
  read_excel("data/Yvoz et al 2021 appendices (1).xlsx", 
             sheet = "Organ level") %>% 
  mutate_if(is.character, str_to_lower) %>% 
  janitor::clean_names() 

#--they have over 150 weeds
yvoz_raw %>% 
  pull(weed_species) %>% 
  unique()

#--do any weeds have more than one organ? No. All species have only one value.
yvoz_raw %>% 
  select(weed_species, floral_organ) %>% 
  distinct() %>% 
  group_by(weed_species) %>% 
  summarise(n = n()) %>% 
  filter(n>1)

yvoz <-
  yvoz_raw %>% 
  mutate(eppo_code = weed_species) 

#--how many of ours have a match? 6 of the 12, but others are species level
d2 <- 
  d1 %>% 
  left_join(cents_species) %>% 
  select(eppo_code, cover_pct, latin_name) %>% 
  left_join(yvoz) %>% 
  arrange(eppo_code)

#--should I use the mean, or the median?
#--ger values, mean woudl be fine
yvoz %>% 
  filter(str_starts(eppo_code, "ger")) %>%
  pivot_longer(pol1:harm3) %>% 
  ggplot(aes(eppo_code, value)) +
  geom_point() +
  facet_wrap(~name, scales = "free")

#--ver values, mean would be fine
yvoz %>% 
  filter(str_starts(eppo_code, "ver")) %>%
  pivot_longer(pol1:harm3) %>% 
  ggplot(aes(eppo_code, value)) +
  geom_point() +
  facet_wrap(~name, scales = "free")

#--lam, 2 values, mean would be fine
yvoz %>% 
  filter(str_starts(eppo_code, "lam")) %>%
  pivot_longer(pol1:harm3) %>% 
  ggplot(aes(eppo_code, value)) +
  geom_point() +
  facet_wrap(~name, scales = "free")

#--sen, 2 values, mean would be fine
yvoz %>% 
  filter(str_starts(eppo_code, "sen")) %>%
  pivot_longer(pol1:harm3) %>% 
  ggplot(aes(eppo_code, value)) +
  geom_point() +
  facet_wrap(~name, scales = "free")

#--cir? 2 values, cents data only has cirar anyways
yvoz %>% 
  filter(str_starts(eppo_code, "cir")) %>%
  pivot_longer(pol1:harm3) %>% 
  ggplot(aes(eppo_code, value)) +
  geom_point() +
  facet_wrap(~name, scales = "free")


# 3. generalize species group values from yvoz data -------------------------------------------------

#--ger mean
d_ger <- 
  yvoz %>% 
  filter(str_starts(eppo_code, "ger")) %>%
  mutate(eppo_code = "gerss") %>% 
  group_by(eppo_code) %>% 
  summarise_at(vars(pol1:harm3), mean, na.rm = T)

#--ver values, take the mean
d_ver <- 
  yvoz %>% 
  filter(str_starts(eppo_code, "ver")) %>%
  mutate(eppo_code = "verss") %>% 
  group_by(eppo_code) %>% 
  summarise_at(vars(pol1:harm3), mean, na.rm = T)

#--2 sen values, take the mean
d_sen <- 
  yvoz %>% 
  filter(str_starts(eppo_code, "sen")) %>%
  mutate(eppo_code = "senss") %>% 
  group_by(eppo_code) %>% 
  summarise_at(vars(pol1:harm3), mean, na.rm = T)

#--lam, 2 values, mean
d_lam <- 
  yvoz %>% 
  filter(str_starts(eppo_code, "lam")) %>%
  mutate(eppo_code = "lamss") %>% 
  group_by(eppo_code) %>% 
  summarise_at(vars(pol1:harm3), mean, na.rm = T)

yvoz_supp <-
  d_ger %>% 
  bind_rows(d_ver) %>% 
  bind_rows(d_sen) %>% 
  bind_rows(d_lam) %>% 
  bind_rows(yvoz)

yvoz_supp


# 4. deal with cover crop values ------------------------------------------

#--barley, nothing
yvoz %>% 
  filter(str_starts(eppo_code, "hor"))
  
#--oats avesa isn't the same as avefa (wild oat), don't use it
yvoz %>% 
  filter(str_starts(eppo_code, "ave"))

#--radish, rapsr, use it
d_rap <- 
  yvoz %>% 
  filter(str_starts(eppo_code, "rap")) %>% 
  mutate(eppo_code = "rapsr")

#--lolium, lolss, use it
d_lol <- 
  yvoz %>% 
  filter(str_starts(eppo_code, "lol")) %>% 
  mutate(eppo_code = "lolpe")

yvoz_supp2 <- 
  d_rap %>% 
  bind_rows(d_lol) %>% 
  bind_rows(yvoz_supp)



# 5. look at absoluate values -----------------------------------------

c1 <- 
  d1 %>% 
  left_join(cents_species) %>% 
  select(eppo_code, cover_pct, latin_name) %>% 
  left_join(yvoz_supp2)

d5 <- 
  c1 %>% 
  select(-weed_species, -floral_organ) %>% 
  pivot_longer(pol1:harm3) %>% 
  mutate(value2 = ifelse(is.na(value), 0, value))
  
#--clean it up to write 'raw' dis-aggregated values
d5a <- 
  d5 %>% 
  select(eppo_code, latin_name, eco_service = name, raw_value = value2) %>% 
  filter(eppo_code != "soil") 

d5a %>% 
  write_csv("data/tidy_spvalue-raw.csv")

library(tidytext)
#--rapsr is the best? always? not ALWAYS
d5 %>% 
  filter(name !="harm3") %>% 
  ungroup() %>% 
  mutate(name = as.factor(name),
         eppo_code = reorder_within(eppo_code, value2, name)) %>% 
  ggplot(aes(eppo_code, value2)) +
  geom_point() +
  scale_x_reordered() +
  facet_wrap(~ name, scales = "free") +
  coord_flip() +
  theme_bw()


# 6. scale the values -----------------------------------------------------

#--scale within a service category
d6 <- 
  d5 %>% 
  group_by(name) %>% 
  mutate(maxval = max(value2),
         sc_value2 = value2/maxval) #--max value should be 1

#--we are good
d6 %>% 
  ggplot(aes(eppo_code, sc_value2)) +
  geom_point() +
  facet_wrap(~name) +
  coord_flip()


# 7. summarise within a category -------------------------------------------------------------
#--summing them (same as taking the average)

d7 <- 
  d6 %>% 
  mutate(cat = case_when(
    grepl("pol", name) ~ "pollinator",
    grepl("cont", name) ~ "foodweb",
    grepl("harm", name) ~ "harm",
    TRUE ~ name)) %>% 
  group_by(eppo_code, latin_name, cat) %>% 
  summarise(sc_value2_sum = sum(sc_value2)) %>% 
  filter(cat != "harm")

#--max should be 3, we are good
d7 %>% 
  ggplot(aes(eppo_code, sc_value2_sum)) +
  geom_point() +
  facet_wrap(~cat)

#--put them in supplemental table
d7 %>% 
  filter(cat == "pollinator")

d7 %>% 
  filter(cat != "foodweb")

#--look at the weeds
#--get the order I want them in
d7.order <- 
  d7 %>%
  filter(cat == "foodweb") %>% 
  arrange(sc_value2_sum) %>% 
  ungroup() %>% 
  mutate(n = 1:n()) %>% 
  select(eppo_code, n) %>% 
  distinct()
  


# 8. correlation? ---------------------------------------------------------

#--relationship btwn categories?
d8 <- 
  d7 %>% 
  pivot_wider(names_from = cat, values_from = sc_value2_sum)

cor(d8$foodweb, d8$pollinator)


# 9. max value ------------------------------------------------------------

#--summed value of foodweb and pollitor is the ecological value
d8 <- 
  d7 %>% 
  group_by(eppo_code, latin_name) %>% 
  summarise(pot_value = sum(sc_value2_sum))

#--write this data 
d8 %>% 
  write_csv("data/tidy_spvalue.csv")


