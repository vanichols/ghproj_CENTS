# created 6 dec
# assign weed disservices 
# based on yvoz
# 13 june 2025 - update values to use 'organ' level

library(tidyverse)
library(readxl)
library(CENTSdata)

cents_species

cents_fallpctcover

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

#--we only have 16 things to assign values to


# 2. explore methods ------------------------------------------------------

#--storkey data is G. species
#--have to change it if I want to match them

st <- 
  read_excel("data/storkey-weed-disservice-traits.xlsx", skip = 5) %>% 
  janitor::clean_names()

#--do any of the harmful data match? 
st2 <- 
  st %>% 
  select(latin_name = weed_species, competitive_index) %>% 
  filter(!is.na(latin_name))

#--No, none of them are our weeds
d1 %>% 
  left_join(cents_species) %>% 
  select(eppo_code, cover_pct, latin_name) %>% 
  left_join(st2)


#--yvoz et a. 2021
#--plant level, mean
# yvoz <- 
#   read_excel("data/Yvoz et al 2021 appendices (1).xlsx", 
#                    sheet = "Plant level") %>% 
#   mutate_if(is.character, str_to_lower) %>% 
#   mutate(eppo_code = Species) %>% 
#   group_by(eppo_code) %>% 
#   summarise_at(vars(Pol1:Harm3), mean, na.rm = T)

#--yvoz et a. 2021
#--organ level, mean?
yvoz_raw <- 
  read_excel("data/Yvoz et al 2021 appendices (1).xlsx", 
             sheet = "Organ level") %>% 
  mutate_if(is.character, str_to_lower) %>% 
  janitor::clean_names() 

#--over 150 weeds
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



# 5. combine, all values assigned -----------------------------------------

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


# 6. scale and add --------------------------------------------------------

#--rapsr is an outlier for p_cont values
d5 %>% 
  ggplot(aes(eppo_code, value2)) +
  geom_point() +
  facet_wrap(~ name, scales = "free") +
  coord_flip()

d6 <- 
  d5 %>% 
  group_by(name) %>% 
  mutate(maxval = max(value2),
         sc_value2 = value2/maxval) #--make value should be 1

#--we are good
d6 %>% 
  ggplot(aes(eppo_code, sc_value2)) +
  geom_point() +
  facet_wrap(~name) +
  coord_flip()


# 7. summarise within a category -------------------------------------------------------------
#--harm has only one value, others have three
#--should I take the max instead? it is potential, after all

d7 <- 
  d6 %>% 
  mutate(cat = case_when(
    grepl("pol", name) ~ "pollinator",
    grepl("cont", name) ~ "ecocont",
    grepl("harm", name) ~ "harm",
    TRUE ~ name)) %>% 
  group_by(eppo_code, latin_name, cat) %>% 
  summarise(sc_value2_max = max(sc_value2)) %>% 
  mutate(sc_value2_max = ifelse(cat == "harm", -sc_value2_max, sc_value2_max))

#--max should be 1, we are good
d7 %>% 
  ggplot(aes(eppo_code, sc_value2_max)) +
  geom_point() +
  facet_wrap(~cat)

#--look at the weeds
#--this is a good figure
d7 %>%
  ggplot(aes(eppo_code, sc_value2_max)) +
  geom_point(aes(color = cat), size = 3) +
  scale_color_manual(values = c("purple", "red", "green4")) +
  coord_flip()

#--write this data 
d7 %>% 
  write_csv("data/tidy_spvalue.csv")
