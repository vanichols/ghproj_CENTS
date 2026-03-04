#--spring weed counts
#--creatd 9 oct 2025
#--something is goofy - the model estimates don't match the figure 
#--model says inv has a mean of like 500 pl m2...
#--4 march 2025 remaking after consulting Simon on model

library(tidyverse)
library(CENTSdata)
library(patchwork)
library(ggh4x)
library(ggrepel)
library(ggpubr)
library(ggpattern)
library(ggbeeswarm)

rm(list = ls())


# fig stuff ---------------------------------------------------------------

source("code/00_color-palettes.R")

theme_set(theme_bw())

th1 <-   theme(axis.ticks.y = element_blank(),
               axis.text.x = element_text(angle = 45, hjust = 1),
               legend.position = "top",
               strip.background.x = element_rect(fill = "white", 
                                                 color = "black"),
               strip.text.x = element_text(size = rel(1.3)),
               panel.border = element_blank())



# 1. data -----------------------------------------------------------------

d0 <- 
  read_csv("data/stats/emmeans/emmeans-spweedcounts.csv")

#--I have to do some trickery to get the stacked bars
d_p <- 
  d0 |> 
  filter(weed_type2 == "P") |> 
  mutate(p_weeds = estimate) |> 
  dplyr::select(till_id, cctrt_id, p_weeds)

d1 <- 
  d0 |> 
  left_join(d_p) |> 
  mutate(estimate2 = ifelse(weed_type2 == "Total", estimate - p_weeds, estimate)) |> 
  dplyr::select(-df, -t.ratio, -p.value) |> 
  filter(weed_type2 != "A")

d2 <- 
  d1 %>%
  MakeNiceLabels(.) |> 
  mutate(weed_type_nice = ifelse(weed_type2 == "Total", "Annual weeds", "Perennial weeds")) 
  

#--this needs to be fixed, somehow indicate tot and P separately?
d3a <- 
  d2 %>% 
  mutate(star1 = ifelse( (cctrt_nice == "MixE" & till_nice == "No-till"), 
                        "*", " ")) %>% 
  group_by(till_nice, cctrt_id) %>% 
  mutate(tot_weeds = sum(estimate2)) |> 
  group_by(till_nice) |> 
  mutate(starpos1 = max(tot_weeds+20))

d3b <- 
  d2 %>% 
  filter(weed_type2 == "P") %>% 
  mutate(star2 = ifelse( (cctrt_nice == "MixE" & till_nice != "Inv"), 
                         "*", " ")) %>% 
  group_by(till_nice, weed_type2) %>% 
  mutate(starpos2 = max(estimate2)+10)
  
# 2. fig ------------------------------------------------------------------

plot1 <- 
  d2 %>% 
  ggplot(aes(cctrt_nice, estimate2)) +
  geom_col(aes(fill = weed_type_nice), color = "black") +
  geom_linerange(aes(ymin = estimate2-SE, ymax = estimate2+SE)) +
  geom_text(data = d3a, 
            aes(x = cctrt_nice, y = starpos1, label = star1),
            size = 8) +
  geom_text(data = d3b, 
            aes(x = cctrt_nice, y = starpos2, label = star2),
            color = bv2, size = 8) +
  facet_grid(~till_nice) +
  scale_fill_manual(values = c(bv3, bv2)) +
  labs(x = "Cover crop system", 
       y = myweedcountlab, 
       fill = NULL) +
  th1 +
  theme(legend.key = element_rect(fill = "transparent",
                                  colour = NA), 
        legend.background = element_rect(fill = "transparent",
                                  colour = NA), 
        legend.position = c(0.2, 0.9))


plot1

ggsave("figs/fig4_spring-weeds.png",
       width = 7, height = 8)



# 3. faba yields vs p weeds----------------------------------------------

y0 <- 
  cents_cropyields %>% filter(crop == "faba bean") |> 
  left_join(cents_eukey) |> 
  mutate(year = year(date2), 
         yearF = paste0("Y", year)) |> 
  group_by(yearF, till_id, straw_id, cctrt_id, crop) |> 
  summarise(yield_dry_Mgha = mean(yield_dry_Mgha))


d4 <- read_csv("data/stats/emmeans/emmeans-spweedcounts-estimates-for-all.csv")


d5 <- 
  y0 |> 
  left_join(d4, by = c("yearF", "till_id", "straw_id", "cctrt_id")) |> 
  mutate(weed_type2 = ifelse(is.na(weed_type2), "Total", weed_type2), 
         weed_type_nice = case_when(
           weed_type2 == "A" ~ "Annual",
           weed_type2 == "P" ~ "Perennial",
           weed_type2 == "Total" ~ "Total",
           TRUE ~ NA
         ))

plot2 <- 
  d5 %>% 
  ggplot(aes(estimate, yield_dry_Mgha)) +
  geom_point(aes(color = weed_type_nice, shape = weed_type_nice), 
             show.legend = F, size = 4) +
  geom_smooth(method = "lm", se = F, color = hue_radl) +
  scale_color_manual(values = c(bv1, bv2, "black")) +
  labs(x = myweedcountlab,
       y = myyieldlab_faba,
       color = NULL,
       shape = NULL) +
  facet_grid(.~ weed_type_nice, scale = "free") +
  th1


plot2
ggsave("figs/arxiv/sfig_spweed-yield-corr.png",
       width = 7, height = 8)

