#--make a circular barplot
#//look at 6 perfomance categories together

#--need to run with updated emmeans values for fig
#--need to increase distance between left axis label and figure text (Saf is covered)
#--change to res+ and res- (+res and -res right now)

rm(list = ls())

library(tidyverse)
library(CENTSdata)
library(ggh4x)
library(patchwork)

source("code/00_color-palettes.R")

# 1. data -----------------------------------------------------------------

d1 <- 
  read_csv("data/tidy_multicriteria-values.csv")


# 2. labels for metrics -------------------------------------------------------------

d1 %>% 
  pull(name) %>% 
  unique()

d2 <- 
  d1 %>% 
  mutate(name_nice = case_when(
    name == "fallvegvalue" ~ "Ecol",
    name == "fallbio" ~ "Biom",
    name == "yield" ~ "Grai",
    name == "minitox" ~ "Safe",
    name == "perennweeds" ~ "Weed",
    name == "pctcov" ~ "Soil",
  ))


# 3. make nice lables cc ------------------------------------------------

d3 <- 
  MakeNiceLabels(d2) %>%  
  mutate(straw_nice = ifelse(straw_id == "retained", "(+)Residue", "(-)Residue"),
         till_nice = case_when(
           till_nice == "Inv"~"Inversion",
           till_nice == "Surf"~"Surface",
           TRUE ~ "No-till"
         ),
         till_nice = factor(till_nice, 
                            levels = c("Inversion", "Surface", "No-till")))


# 4. total score ----------------------------------------------------------

d4 <- 
  d3 %>% 
  group_by(pick(contains("nice"), -name_nice)) %>% 
  summarise(value_smy = mean(value))
  

# 5. fig ------------------------------------------------------------------

d5 <- 
  d3 %>% 
  left_join(d4)


d_smy <- 
  d5 |> 
  select(cctrt_nice, till_nice, straw_nice, value_smy) |> 
  distinct() |> 
  mutate(value_smy = round(value_smy, 2))

p1 <- 
  d5 %>% 
  ggplot(aes((str_wrap(name_nice, 5)), value)) +
  geom_col(aes(fill = value_smy),
           color = "black") +
  geom_text(data = d_smy, 
            aes(x = 0.5, y = -0.5, label = value_smy),
           color = "black") +
  scale_fill_viridis_c() +
  scale_y_continuous(limits = c(-.5, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_x_discrete(expand = c(0, 0)) +  # Remove x-axis padding
  facet_nested(cctrt_nice~till_nice + straw_nice,
               switch = "y", 
               strip = strip_nested(
                 text_x = list(
                   element_text(size = rel(1.3), "bold"),  # till_nice (outer)
                   element_text(size = rel(1.3), "italic")    # straw_nice (inner)
                 ),
                 text_y = element_text(size = rel(1.3))  # cctrt_nice (rows)
               )
  ) +
  coord_polar(clip = "off") +
  labs(x = NULL,
       y = NULL,
       fill = "Total relative value") +
  theme_bw() +
  theme(axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.border = element_blank(),
        legend.position = "Right",
        legend.title.position = "top",
        strip.background.x = element_rect(fill = "transparent", 
                                          color = "white"),
        strip.background.y = element_rect(fill = "transparent", 
                                          color = "white"),
        plot.margin=unit(c(1,1,1.5,1.2),"cm")) 


# second plot -------------------------------------------------------------

d6 <- 
  d_smy |> 
  mutate(straw_nice2 = str_remove(straw_nice, "Residue"),
         sys_nice = paste0(till_nice, straw_nice2)) |> 
  select(cctrt_nice, sys_nice, value_smy) |> 
  group_by(sys_nice) |>
  arrange(-value_smy) |> 
  mutate(rank = 1:n()) |> 
  arrange(sys_nice)

p2 <- 
  d6 %>% 
  ggplot(aes(sys_nice, rank, color = cctrt_nice, group = cctrt_nice)) +
  geom_line(size = 2) +
  geom_point(size = 6) +
  scale_y_reverse() +
  guides(col = guide_legend(reverse = TRUE)) +
  labs(x = NULL,
       y = "Rank",
       color = "Cover crop system") +
  scale_color_manual(values = c( 
    "NoCC" = hue_nocc,
    "MixE" = hue_mixe,
    "MixM" = hue_mixm,
    "RadM" = hue_radm,
    "RadL" = hue_radl)) +
  theme_bw() +
  theme(axis.ticks = element_blank(), 
        panel.spacing = unit(1, "lines"),
        panel.border = element_blank(),
        legend.position = "right",
        legend.title.position = "top") 



# combo -------------------------------------------------------------------

p1 / p2

ggsave("figs/fig5_total-value-donut.png", height = 10, width = 10)
