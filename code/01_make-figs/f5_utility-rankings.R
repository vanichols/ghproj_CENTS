#--show rank changes of farmer typologies
#--created 22 aug 2024

rm(list = ls())

theme_set(theme_bw())
library(tidyverse)
library(CENTSdata)
library(ggh4x)
library(patchwork)
library(ggalluvial)
library(plotrix)

source("code/00_color-palettes.R")

# 1. data -----------------------------------------------------------------

d1a <- read_csv("data/tidy_typology-values.csv")

d1 <- 
  d1a %>% 
  separate(cropsys, into = c("till_id", "cctrt_id", "straw_id"), sep = " ", remove = F) %>% 
  group_by(farmer_type, till_id) %>% 
  arrange(-valuetot) %>% 
  mutate(rank = 1:n()) %>% 
  arrange(farmer_type, rank)

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
  mutate(straw_nice = ifelse(straw_id == "retained", "+Res", "-Res"),
         farmer_type = factor(farmer_type, 
                                 levels = c("Conservationist",
                                            "Deliberative",
                                            "Traditionalist",
                                            "Productionist"))) 


#--

av1 <-"#A50026"
av2 <-"#FDAE61"
av3 <-"#FFFFBF"
av4 <- "#ABD9E9"
av5 <- "#313695"

d4 %>% 
  ggplot(aes(farmer_type, rank)) +
  geom_line(aes(group = cropsys, color = cctrt_nice, alpha = straw_nice), 
            linewidth = 2) +
  scale_y_reverse(breaks = c(10, 8, 6, 4, 2, 1)) +
  scale_color_manual(values = c(
    "No CC" = cv1,
    "Early Mix" = cv5,
    "Mid Mix" = cv4,
    "Mid Rad" = cv3,
    "Late Rad" = cv2)) +
  scale_alpha_manual(values = c(1, 0.6)) +
  labs(color = "Cover crop",
       alpha = "Residue",
       x = NULL,
       y = "Rank") +
  facet_wrap(~till_nice) +
  theme(axis.ticks.y = element_blank(),
        legend.position = "top",
        strip.background.x = element_rect(fill = "white", 
                                          color = "white"),
        strip.text.x = element_text(size = rel(1.3)),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.box = "vertical"
  )

ggsave("figs/fig_ranks.png",
       width = 8, height = 4)

