library(tidyverse)
library(readxl)
library(ggbeeswarm)

d <- read_excel("data/Yvoz et al 2021 appendices (1).xlsx", sheet = "Plant level")

d1 <- 
  d |> 
  pivot_longer(Pol1:Harm3) |> 
  group_by(name) |> 
  mutate(value_sc = value / max(value),
         value_sc = ifelse(grepl("Harm", name), 1- value_sc, value_sc))

fig1 <- 
  d1 |> 
  ggplot() +
  geom_jitter(aes(name, value_sc, color = Species), show.legend = F) 

  
fig2 <-
  d1 |> 
  filter(Species == "ACHMI") |> 
  group_by(Species, name) |> 
  mutate(value_sc_mean = mean(value_sc)) |> 
  ggplot() +
  geom_point(aes(name, value_sc)) +
  geom_col(aes(name, value_sc_mean)) +
  coord_polar()


# 
# d1 |> 
#   ggplot(aes(name, value_sc)) +
#   geom_point()
# 
# d2 <- 
#   d1 |> 
#   select(-value) |> 
#   unite(Crop, Location, col = "Env")
# 
# d3 <- 
#   d2 |> 
#   group_by(Species, name) |> 
#   summarise(value_sc_mean = mean(value_sc))
# 
# d4 <- 
#   d2 |> 
#   pivot_wider(names_from = Env, values_from = value_sc) |> 
#   left_join(d3)
# 
# species_list <- 
#   d4 |> 
#   pull(Species) |> 
#   unique()
# 
# d1  |> 
#   filter(name == "Pol1" & value_sc > 0.5)
#   
# 
# sub_species_list <- species_list[c(1, 4, 6, 19, 24, 36)]
# 
# d4 |> 
#   filter(Species %in% sub_species_list) |> 
#   ggplot() +
#   geom_col(aes(name, value_sc_mean)) +
#   coord_polar() +
#   facet_grid(.~Species)
