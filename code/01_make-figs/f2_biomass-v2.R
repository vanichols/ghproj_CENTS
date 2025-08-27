#--first understand if biomass and pct cover are related

library(tidyverse)
library(CENTSdata)
library(patchwork)
library(ggh4x)

rm(list = ls())

source("code/00_color-palettes.R")

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_fallbio)
c <- 
  as_tibble(cents_cropyields) %>% 
  mutate(year = year(date2)) %>% 
  dplyr::select(year, crop)

w <- 
  read_csv("data/tidy_weaclass.csv")

b_stats <- read_csv("data/stats_em_fallbio.csv")
# were biomass and cover correlated ---------------------------------------
#--get average coverage for each catgory by plot

#--get biomass into same format
d_bio <- 
  y %>% 
  mutate(dm_gm2 = ifelse(is.na(dm_gm2), 0, dm_gm2)) %>% 
  mutate(dm_cat = case_when(
    dm_type == "grass_cl" ~ "covercrop",
    dm_type == "radish" ~ "covercrop",
    TRUE ~ "other"
  ),
  year = year(date2)) %>% 
  group_by(eu_id, year, dm_cat) %>% 
  summarise(dm_gm2 = sum(dm_gm2))
  
#--not really linear, worth showing separately?
# d_bio %>% 
#   left_join(d_pct) %>% 
#   ggplot(aes(dm_gm2, cover_pct)) +
#   geom_point(aes(color = dm_cat)) 


# 1. biomass linked to trts ---------------------------------------------------------------------
d1 <- 
  d_bio %>% 
  left_join(eu) %>% 
  left_join(w) %>% 
  distinct()

# 2. make nice lables cc ------------------------------------------------

d2 <- 
  d1 %>% 
  mutate(cctrt_nice = case_when(
           cctrt_id == "nocc" ~ "No CC",
           cctrt_id == "mix_E" ~ "Early Mix",
           cctrt_id == "mix_M" ~ "Mid Mix",
           cctrt_id == "rad_M" ~ "Mid Rad",
           cctrt_id == "rad_L" ~ "Late Rad",
           TRUE~"XXX"
         ),
         cctrt_id = factor(cctrt_id, levels = ord.cctrt_id),
         cctrt_nice = factor(cctrt_nice, levels = ord.cctrt_niceNEW))


# 3. make nice tillage ----------------------------------------------------

d3 <- 
  d2 %>% 
  mutate(
    till_id = factor(till_id, levels = ord.till_id),
         till_nice = case_when(
           till_id == "notill" ~ "No-till",
           till_id == "inversion" ~ "Inv",
           till_id == "surface" ~ "Surf",
           TRUE ~ "XXX"
         ),
         till_nice = factor(till_nice, levels = ord.till_nice))


# 4. make nice straw, weather column combo and dm cat -----------------------------

d4 <- 
  d3 %>% 
  mutate(precip = str_to_title(precip),
         year_prec = paste0("(", year, ") ", precip),
         dm_cat = ifelse(dm_cat == "covercrop", "Cover crop", "Other")) 

# 5. group  ---------------------------------------------------------------

d5a <-
  d4 %>%  
  group_by(cctrt_id, till_id, year, year_prec,
           cctrt_nice, till_nice, precip) %>% 
  summarise(dm_gm2 = mean(dm_gm2)) 

#--keep dm cat
d5 <-
  d4 %>%  
  group_by(dm_cat,
           cctrt_id, till_id, year, year_prec,
           cctrt_nice, till_nice, precip) %>% 
  summarise(dm_gm2 = mean(dm_gm2)) 

# 6. merge w-stats --------------------------------------------------------

#--should be a straight line
d6a <- 
  d5a %>% 
  left_join(b_stats, relationship = "many-to-many") %>% 
  mutate(letters = as.character(.group),
         letters = ifelse(year == 2019, str_to_lower(letters), letters))

d6a %>% 
  ggplot(aes(dm_gm2, estimate)) +
  geom_point()

d6 <- 
  d5 %>% 
  left_join(b_stats, relationship = "many-to-many")
  
ggplot() +
  geom_col(data = d6, 
           aes(till_nice, dm_gm2*0.01, fill = dm_cat),
           color = "black") +
  geom_linerange(data = d6a, 
                aes(x = till_nice, 
                    ymin = conf.low*0.01,
                    ymax = conf.high*0.01),
                color = "white") +
  geom_text(data = d6a, aes(x = till_nice, 
                             y = estimate*0.01 + 0.5, 
                             label = letters),
            size = 2) +
  facet_nested(year_prec  ~ cctrt_nice) +
  scale_fill_manual(values = c("gold", "darkblue")) +
  labs(x = "Tillage",
       y = mybmlab, 
       fill = NULL) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        legend.position = "top",
        strip.background.x = element_rect(fill = "white", 
                                          color = "white"),
        strip.text.x = element_text(size = rel(1.3)),
        panel.border = element_blank()
        )

ggsave("figs/fig_fall-biomass.png", width = 8, height = 5)

# other versions ----------------------------------------------------------


d1 %>%
  mutate(
    straw_thing = ifelse(straw_id == "retained", "+", "-"),
    wea_straw = paste0(precip, straw_thing)) %>% 
  group_by(cctrt_id, dm_cat, wea_straw, till_id, precip, straw_id) %>% 
  summarise(dm_gm2 = mean(dm_gm2)) %>% 
  ggplot(aes(cctrt_id, dm_gm2)) +
  geom_col(aes(fill = dm_cat)) +
  facet_grid(precip + straw_id  ~ till_id) +
  scale_fill_manual(values = c("gold", "darkblue"))

