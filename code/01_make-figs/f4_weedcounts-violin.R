# created 11 may 2025
# purpose: visualize spring weed counts
# notes: 3 subreps

devtools::install_github("psyteachr/introdataviz")

library(tidyverse)
library(CENTSdata)
library(ggh4x)
library(introdataviz)

rm(list = ls())

source("code/00_color-palettes.R")

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_spweedcount)
w <- read_csv("data/tidy_weaclass.csv")


# 1. data -----------------------------------------------------------------

draw <- 
  y %>% 
  mutate(year = year(date2),
         yearF = paste("Y", year)) %>% 
  left_join(eu) %>% 
  left_join(w) %>% 
  mutate(weayear = paste(year, precip, sep = "-"),
         weed_type2 = ifelse(weed_type %in% c("dicot", "monocot"), "A", "P"))


#--get one value per block for P and A weeds
d_blockAP <- 
  draw %>% 
  group_by(year, yearF, precip,
           weed_type2,
           block_id,  
           straw_id, till_id, cctrt_id, 
           weayear) %>% 
  summarise(count = sum(count)) %>% 
  ungroup()

#--get one value per block
d_block <- 
  d_blockAP %>% 
  group_by(year, yearF, precip, 
           block_id,  
           straw_id, till_id, cctrt_id, 
           weayear) %>% 
  summarise(count = sum(count)) %>% 
  ungroup()


#--get mean over all blocks
d_mean <- 
  d_block %>% 
  group_by(year, yearF, precip,cctrt_id, till_id, straw_id) %>%
  summarise(count = mean(count, na.rm = T)) 

#--get mean over all blocks for each A and P
d_meanAP <- 
  d_blockAP %>% 
  group_by(year, yearF, precip, weed_type2, cctrt_id, till_id, straw_id) %>%
  summarise(count = mean(count, na.rm = T)) 

#--use the blocks
d1 <- d_blockAP

# 2. make nice lables cc ------------------------------------------------

d2 <- 
  d1 %>% 
  mutate(cctrt_nice = case_when(
    cctrt_id == "nocc" ~ "NoCC",
    cctrt_id == "mix_E" ~ "MixEarly",
    cctrt_id == "mix_M" ~ "MixMid",
    cctrt_id == "rad_M" ~ "RadMid",
    cctrt_id == "rad_L" ~ "RadLate",
    TRUE~"XXX"
  ),
  cctrt_id = factor(cctrt_id, levels = ord.cctrt_id),
  cctrt_nice = factor(cctrt_nice, levels = ord.cctrt_niceL),
  cctrt_nice = fct_rev(cctrt_nice))


# 3. make nice tillage ----------------------------------------------------

d3 <- 
  d2 %>% 
  mutate(
    till_id = factor(till_id, levels = ord.till_id),
    till_nice = case_when(
      till_id == "notill" ~ "No-till",
      till_id == "inversion" ~ "Inversion",
      till_id == "surface" ~ "Surface",
      TRUE ~ "XXX"
    ),
    till_nice = factor(till_nice, levels = ord.till_nice2))


# 4. make nice straw, weather column  -----------------------------

d4 <- 
  d3 %>% 
  mutate(precip = str_to_title(precip),
         year_prec = paste0("(", year, ") ", precip))



# 5. get number of points -------------------------------------------------

d4 %>% 
  group_by(till_id, cctrt_id) %>% 
  summarise(n = n())

# 5. violin plot ----------------------------------------------------------

d4all <- 
  d4 %>% 
  group_by(year, yearF, precip, block_id, straw_id, till_id, cctrt_id, weayear, cctrt_nice, till_nice, year_prec) %>% 
  summarise(count = sum(count))

ggplot() +
  geom_violin(data = d4all,
              aes(cctrt_nice, count, fill = cctrt_nice),
              show.legend = F) +
  facet_grid(till_nice~.) +
  scale_fill_manual(values = c("tan","tan","tan","tan", "black"))+
  labs(x = "Cover crop treatment",
       y = myweedcountlab) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        legend.position = "top",
        strip.background.x = element_rect(fill = "white", 
                                          color = "white"),
        strip.text.y = element_text(size = rel(1.3), angle = 0),
        #panel.border = element_blank()
  ) +
  coord_flip()

ggsave("figs/fig_spring-weed-counts.png",
       width = 6, height = 6)



# 6. violin plot sep perenn----------------------------------------------------------


ggplot() +
  geom_split_violin(data = d4,
              aes(cctrt_nice, count, fill = weed_type2),
              show.legend = F) +
  facet_grid(till_nice~.) +
  #scale_fill_manual(values = c("tan","tan","tan","tan", "black"))+
  labs(x = "Cover crop treatment",
       y = myweedcountlab) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        legend.position = "top",
        strip.background.x = element_rect(fill = "white", 
                                          color = "white"),
        strip.text.y = element_text(size = rel(1.3), angle = 0),
        #panel.border = element_blank()
  ) +
  coord_flip()

ggsave("figs/fig_spring-weed-counts.png",
       width = 6, height = 6)

