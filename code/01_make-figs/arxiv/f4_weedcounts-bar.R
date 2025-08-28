# created 11 may 2025
# purpose: visualize spring weed counts
# notes: 3 subreps

#devtools::install_github("psyteachr/introdataviz")

library(tidyverse)
library(CENTSdata)
library(ggh4x)
#library(introdataviz)

rm(list = ls())

source("code/00_color-palettes.R")

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_spweedcount)
w <- read_csv("data/tidy_weaclass.csv")

res <- 
  read_csv("data/stats_emmeans/emmeans-spweedcounts.csv") %>% 
  select(cctrt_id, till_id, count_stats = response, se = std.error) %>% 
  mutate(lo = count_stats - se,
         hi = count_stats +se)

# 1. data -----------------------------------------------------------------

draw <- 
  y %>% 
  mutate(year = year(date2),
         yearF = paste("Y", year)) %>% 
  left_join(eu) %>% 
  left_join(w) %>% 
  mutate(weayear = paste(year, precip, sep = "-"),
         weed_type2 = ifelse(weed_type %in% c("dicot", "monocot"), "A", "P"))

#--hope emmeans matches!
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

#--get mean over straw and year
d_want <-
  d_blockAP %>% 
  group_by(cctrt_id, till_id, weed_type2) %>% 
  summarise(count = mean(count))

#--use what I want
d1 <- 
  d_want %>% 
  bind_rows(
    d_want %>% 
      group_by(till_id, cctrt_id) %>% 
      summarise(count = sum(count)) %>% 
      mutate(weed_type2 = "all")
  )
  

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
  cctrt_nice = factor(cctrt_nice, levels = ord.cctrt_niceNEW),
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


# 4. bar plot ----------------------------------------------------------

d4 <- 
  d3 %>%
  mutate(weed_type3 = ifelse(weed_type2 == "A", "Annual", 
                             ifelse(weed_type2 == "P", "Perennial", "All")),
         star = ifelse(cctrt_nice == "Early Mix" & till_id == "notill",
                       "*", " ")) %>% 
  left_join(res) %>% 
  mutate(lo = count - se,
         hi = count + se)

d4 %>% 
  group_by(cctrt_id, till_id) %>% 
  summarise(count = sum(count),
            count_stats = mean(count_stats)) %>% 
  ggplot(aes(count, count_stats)) +
  geom_point()

#--can I show stars?
#--yes, only mix_e in no-till

ggplot() +
  geom_col(data = d4 %>% filter(weed_type2 %in% c("A", "P")),
              aes(cctrt_nice, count, fill = weed_type3)) +
  geom_errorbar(data = d4 %>% filter(weed_type3 == "All"),
           aes(cctrt_nice, ymin = lo, ymax = hi)) +
  geom_text(data = d4 %>% filter(weed_type3 == "Annual"), 
             aes(cctrt_nice, count + 100, label = star),
            vjust = 0.5) +
  facet_grid(till_nice~.) +
  scale_fill_manual(values = c("orange","black"))+
  labs(x = "Cover crop treatment",
       y = myweedcountlab,
       fill = "Weed growth habit") +
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

ggplot() +
  geom_col(data = d4,
           aes(till_nice, count, fill = weed_type2)) +
  facet_grid(cctrt_nice~.) +
  scale_fill_manual(values = c("orange","black"))+
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

