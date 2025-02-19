# created 6 dec
# assign weed disservices 
# based on storkey 2006

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


#--yvoz et a. 2021?
yvoz <- 
  read_excel("data/Yvoz et al 2021 appendices (1).xlsx", 
                   sheet = "Plant level") %>% 
  mutate_if(is.character, str_to_lower) %>% 
  mutate(eppo_code = Species) %>% 
  group_by(eppo_code) %>% 
  summarise_at(vars(Pol1:Harm3), mean, na.rm = T)

#--over 150 weeds
yvoz %>% 
  pull(eppo_code) %>% 
  unique()

#--how many of ours have a match?
d2 <- 
  d1 %>% 
  left_join(cents_species) %>% 
  select(eppo_code, cover_pct, latin_name) %>% 
  left_join(yvoz) 

#--ger values, take the average? or the median?
#--ver values, take the median
#--2 sen values, they are pretty differnt, refine it later
#--lam, 5 values, also pretty different

yvoz %>% 
  filter(grepl("lam", eppo_code)) %>%
  pivot_longer(Pol1:Harm3) %>% 
  ggplot(aes(eppo_code, value)) +
  geom_point() +
  facet_wrap(~name, scales = "free")

yvoz %>% 
  filter(grepl("sen", eppo_code)) %>%
  pivot_longer(Pol1:Harm3) %>% 
  ggplot(aes(eppo_code, value)) +
  geom_point() +
  facet_wrap(~name, scales = "free")

d1 %>% 
  left_join(cents_species) %>% 
  select(eppo_code, cover_pct, latin_name) %>% 
  left_join(yvoz) 


# 3. supplement yvoz data -------------------------------------------------

#--change cirss to cirar for merge
d_cir <- 
  yvoz %>% 
  filter(grepl("cirar", eppo_code)) #%>% #not necessary now
  #mutate(eppo_code = "cirss")

#--ger values, take the average? or the median?
d_ger <- 
  yvoz %>% 
  filter(grepl("ger", eppo_code)) %>%
  mutate(eppo_code = "gerss") %>% 
  group_by(eppo_code) %>% 
  summarise_at(vars(Pol1:Harm3), median, na.rm = T)

#--ver values, take the median
d_ver <- 
  yvoz %>% 
  filter(grepl("ver", eppo_code)) %>%
  mutate(eppo_code = "verss") %>% 
  group_by(eppo_code) %>% 
  summarise_at(vars(Pol1:Harm3), median, na.rm = T)

#--2 sen values, they are pretty differnt, refine it later
d_sen <- 
  yvoz %>% 
  filter(grepl("sen", eppo_code)) %>%
  mutate(eppo_code = "senss") %>% 
  group_by(eppo_code) %>% 
  summarise_at(vars(Pol1:Harm3), median, na.rm = T)

#--lam, 5 values, also pretty different
d_lam <- 
  yvoz %>% 
  filter(grepl("lam", eppo_code)) %>%
  mutate(eppo_code = "lamss") %>% 
  group_by(eppo_code) %>% 
  summarise_at(vars(Pol1:Harm3), median, na.rm = T)

yvoz_supp <-
  d_cir %>% 
  bind_rows(d_ger) %>% 
  bind_rows(d_ver) %>% 
  bind_rows(d_sen) %>% 
  bind_rows(d_lam) %>% 
  bind_rows(yvoz)

yvoz_supp

d3 <- 
  d1 %>% 
  left_join(cents_species) %>% 
  select(eppo_code, cover_pct, latin_name) %>% 
  left_join(yvoz_supp) %>% 
  pivot_longer(Pol1:Harm3) %>% 
  mutate(value2 = ifelse(is.na(value), 0, value))

d3


# 4. scale and add --------------------------------------------------------

d3 %>% 
  ggplot(aes(eppo_code, value2)) +
  geom_point() +
  facet_wrap(~ name, scales = "free")

d4 <- 
  d3 %>% 
  group_by(name) %>% 
  mutate(maxval = max(value2),
         sc_value2 = value2/maxval) #--make value should be 1

#--we are good
d4 %>% 
  ggplot(aes(eppo_code, sc_value2)) +
  geom_point() +
  facet_wrap(~name)


# 5. sum them -------------------------------------------------------------

d5 <- 
  d4 %>% 
  mutate(cat = case_when(
    grepl("Pol", name) ~ "pollinator",
    grepl("PCont", name) ~ "ecocont",
    grepl("Harm", name) ~ "harm",
    TRUE ~ name)) %>% 
  group_by(eppo_code, latin_name, cat) %>% 
  summarise(sc_value2_sum = sum(sc_value2)) 

#--max should be 3, we are good
d5 %>% 
  ggplot(aes(eppo_code, sc_value2_sum)) +
  geom_point() +
  facet_wrap(~cat)


# 6. average pollin and pcont ---------------------------------------------

d6 <- 
  d5 %>%
  pivot_wider(names_from = cat, values_from = sc_value2_sum) %>% 
  mutate(benef = (ecocont + pollinator)/2,
         net = benef - harm) 


d6 %>% 
  ggplot(aes(harm, benef)) +
  geom_point(aes(color = eppo_code), size = 5)

d6 %>% 
  mutate(color = ifelse(net >= 0, "good", "bad")) %>% 
  ggplot(aes(eppo_code, net)) +
  geom_point(aes(color = color), size = 5)

d6 %>% 
  write_csv("data/tidy_spvalue.csv")
