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

#--has total, p, and a, shouldn\t have year
d0 <- 
  read_csv("data/stats/figs_emmeans/emmeans-fig4-spweedcounts.csv")

#--I have to do some trickery to get the stacked bars
#--get the perennial values
d_p <- 
  d0 |> 
  filter(weed_type2 == "P") |> 
  mutate(p_weeds = estimate) |> 
  dplyr::select(till_id, cctrt_id, p_weeds)

#--subtract them from the total to get the 'annual' number to stack on top of perennial
d1 <- 
  d0 |> 
  filter(weed_type2 != "A") |> 
  left_join(d_p) |> 
  mutate(estimate2 = ifelse(weed_type2 == "Total", estimate - p_weeds, estimate))

d2 <- 
  d1 %>%
  MakeNiceLabels(.) |> 
  mutate(weed_type_nice = ifelse(weed_type2 == "Total", "Annual weeds", "Perennial weeds")) |> 
  arrange(till_nice) |> 
  mutate(till_nice2 = str_to_sentence(till_id),
         till_nice2 = ifelse(till_id == "notill", "No-till", till_nice2),
         till_nice2F = fct_inorder(till_nice2),
         till_nice2F = fct_rev(till_nice2F))
  
  
#--put the 'total' star 30 above the mean value
d3a <- 
  d2 %>% 
  mutate(star1 = ifelse( (cctrt_nice == "MixE" & till_nice2F == "No-till"), 
                        "*", " ")) %>% 
  group_by(till_nice2F, cctrt_id) %>% 
  mutate(tot_weeds = sum(estimate2)) |> 
  group_by(till_nice2F) |> 
  mutate(starpos1 = max(tot_weeds+30))

#--put the perennial starts 10 above the mean value
d3b <- 
  d2 %>% 
  filter(weed_type2 == "P") %>% 
  mutate(star2 = ifelse( (cctrt_nice == "MixE" & till_nice2F != "Inversion"), 
                         "*", " ")) %>% 
  group_by(till_nice2F, weed_type2) %>% 
  mutate(starpos2 = max(estimate2)+10)
  
# 2. fig ------------------------------------------------------------------

#--something is not working
d2 |> 
  arrange(till_id, cctrt_id)

d2 %>%
  ggplot(aes(cctrt_nice, estimate2)) +
  geom_col(aes(fill = weed_type_nice), color = "black") +
  geom_linerange(aes(ymin = estimate-std.error, ymax = estimate+std.error)) +
  geom_text(data = d3a, 
            aes(x = cctrt_nice, y = starpos1, label = star1),
            size = 8) +
  geom_text(data = d3b, 
            aes(x = cctrt_nice, y = starpos2, label = star2),
            color = bv2, size = 8) +
  facet_grid(~till_nice2F) +
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


ggsave("figs/fig4_spring-weeds.png",
       width = 7, height = 8)



