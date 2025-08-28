#--make bar plots to show change in comp to no cc

rm(list = ls())

library(tidyverse)
library(CENTSdata)
library(ggh4x) #--for nested facets
library(patchwork)
library(tidytext) #--for reorder within

source("code/00_color-palettes.R")

# 1. data -----------------------------------------------------------------

d1a <- 
  read_csv("data/tidy_tradeoffs.csv") %>% 
  unite(cctrt_id, till_id, straw_id, col = "sys", remove = F) %>% 
  mutate(value2 = ifelse(name == "pweed_nu", value2 +1, value2))

d1a %>% 
  filter(name == "harm")

#--pick a reference system
dref <- 
  d1a %>% 
  filter(sys == "nocc_surface_removed") %>% 
  rename("ref_val" = value2) %>% 
  select(year, name, cat, ref_val)


#--make nocc the reference sys for each till/straw combo 
drefnocc <- 
  d1a %>% 
  filter(cctrt_id == "nocc") %>% 
  rename("ref_val" = value2) %>% 
  select(year, name, cat, till_id, straw_id, ref_val) %>% 
  distinct()

# 
# #--cal change relative to one ref system
# d1 <- 
#   d1a %>% 
#   left_join(dref) %>% 
#   mutate(val_pctchange = (value2 - ref_val)/ref_val*100,
#          #--use raw change in pweed_nu
#          val_pctchange = ifelse(name == "pweed_nu", value2 - ref_val, val_pctchange))

#--cal change relative to nocc ref sys
d1 <- 
  d1a %>% 
  left_join(drefnocc) %>% 
  mutate(val_pctchange = (value2 - ref_val)/ref_val*100,
         lrr = log(value2/ref_val),
         #--use raw change in pweed_nu
         val_pctchange = ifelse(name == "pweed_nu", value2 - ref_val, val_pctchange),
         lrr = ifelse(name == "pweed_nu", log(value2/(ref_val+1)), lrr))

d1 %>% 
  filter(name == "benef")

# 2. average over years ---------------------------------------------------

d2 <- 
  d1 %>% 
  group_by(sys, till_id, cctrt_id, straw_id, cat, name) %>% 
  summarise(val_pctchange = mean(val_pctchange),
            lrr = mean(lrr))

d2

# 2. fig playing ----------------------------------------------------------

d2 %>% 
  filter(cctrt_id != "nocc") %>% 
  ggplot(aes(cctrt_id, lrr)) +
  geom_col(aes(fill = cat)) +
  facet_grid(till_id + straw_id~name, scales = "free_x") +
  coord_flip()

d2 %>% 
  filter(cctrt_id != "nocc") %>% 
  #filter(till_id == "inversion") %>% 
  filter(straw_id == "removed") %>% 
  ggplot(aes(name, lrr)) +
  geom_col(aes(fill = cat)) +
  facet_grid(cctrt_id~till_id, scales = "free_x") +
  coord_flip()


# 2. labels for metrics -------------------------------------------------------------

d2 <- 
  d2 %>% 
  mutate(name_nice = case_when(
    name == "bio_gm2" ~ "Biomass",
    name == "yield_dry_Mgha" ~ "Grain yields",
    name == "benef" ~ "Ecosys benefits",
    name == "load_ha" ~ "Red. toxicity",
    name == "harm" ~ "Red. future weeds",
    name == "pweed_nu" ~ "Red. perennial weeds",
  )) %>% 
  mutate(name_nice2 = case_when(
    name == "bio_gm2" ~ "Biomass",
    name == "yield_dry_Mgha" ~ "Yields",
    name == "benef" ~ "Ecosys bene",
    name == "load_ha" ~ "Toxicity",
    name == "harm" ~ "Future weeds",
    name == "pweed_nu" ~ "Peren weeds",
  ))

# 3. make nice lables cc ------------------------------------------------
  
d3 <- 
  d2 %>% 
  mutate(cctrt_nice = case_when(
    cctrt_id == "nocc" ~ "No CC",
    cctrt_id == "mix_E" ~ "+early mix",
    cctrt_id == "mix_M" ~ "+mid mix",
    cctrt_id == "rad_M" ~ "+mid rad",
    cctrt_id == "rad_L" ~ "+late rad",
    TRUE~"XXX"
  ),
  cctrt_id = factor(cctrt_id, levels = ord.cctrt_id),
  cctrt_nice = factor(cctrt_nice, levels = ord.cctrt_nicePRES))


# 4. make nice tillage ----------------------------------------------------

d4 <- 
  d3 %>% 
  mutate(
    till_id = factor(till_id, levels = ord.till_id),
    till_nice = case_when(
      till_id == "notill" ~ "No-till",
      till_id == "inversion" ~ "Inv",
      till_id == "surface" ~ "Surf",
      TRUE ~ "XXX"
    ),
    till_nice = factor(till_nice, levels = ord.till_nice))


# 5. make nice straw ----------------------------------------------------

d5 <- 
  d4 %>% 
  mutate(straw_nice = ifelse(straw_id == "retained", "residue retained", "residue removed"))


# 8. figs -----------------------------------------------------------------

#--make things on the right good

d8 <- 
  d5 %>% 
  filter(cctrt_id != "nocc") %>% 
  #filter(till_id == "inversion") %>% 
  filter(straw_id == "retained") %>% 
  mutate(lrr_adj = case_when(
    cat == "service" ~ lrr,
    cat == "dis-service" ~ -lrr,
    TRUE ~ 999
  ),
  colorhelp = ifelse(lrr_adj < 0, "bad", "good"))

d8 %>% 
  ggplot(aes(reorder_within(name_nice2, lrr_adj, list(cctrt_nice, till_nice)),
             lrr_adj)) +
  geom_col(aes(fill = colorhelp), color = "black", show.legend = F) +
  facet_wrap(till_nice ~ straw_nice + cctrt_nice, scales = "free_y", ncol = 4) +
  coord_flip() +
  labs(y = "Log-response-ratio",
       x = NULL) +
    scale_x_reordered() +
  scale_fill_manual(values = c("red4", "dodgerblue")) +
  theme_bw() +
  theme(axis.text.y = element_text(size = rel(1.5), color = "gray"),
        strip.background = element_rect(fill = "tan"),
        strip.text = element_text(size = rel(1.3)))

ggsave("figs/pfig_logrr.png",
       width = 12, height = 6)


# 9. fig zoom in -------------------------------------------------------------

d9 <- 
  d8 %>% 
  filter(till_id == "notill",
         cctrt_nice == "+mid mix", 
         straw_id == "retained")

d9 %>% 
ggplot(aes(reorder_within(name_nice, lrr_adj, list(cctrt_nice, till_nice)),
           lrr_adj)) +
  geom_col(aes(fill = colorhelp), color = "black", show.legend = F) +
  facet_wrap(till_nice ~ straw_nice+cctrt_nice, scales = "free_y", ncol = 4) +
  coord_flip() +
  labs(y = "\nLog-response-ratio",
       x = NULL) +
  scale_x_reordered() +
  scale_fill_manual(values = c("red4", "dodgerblue")) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.5), color = "gray"),
        strip.background = element_rect(fill = "tan"),
        strip.text = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.5)))

ggsave("figs/pfig_logrr-one-panel.png",
       width = 6, height = 4)


# 10. Overwhelm -----------------------------------------------------------------


d10 <- 
  d5 %>% 
  filter(cctrt_id != "nocc") %>% 
  #filter(till_id == "inversion") %>% 
  #filter(straw_id == "retained") %>% 
  mutate(lrr_adj = case_when(
    cat == "service" ~ lrr,
    cat == "dis-service" ~ -lrr,
    TRUE ~ 999
  ),
  colorhelp = ifelse(lrr_adj < 0, "bad", "good"))

d10 %>% 
  ggplot(aes(reorder_within(name_nice2, lrr_adj, list(cctrt_nice, till_nice, straw_nice)),
             lrr_adj)) +
  geom_col(aes(fill = colorhelp), color = "black", show.legend = F) +
  facet_wrap(till_nice ~ straw_nice + cctrt_nice, scales = "free_y", ncol = 4) +
  coord_flip() +
  labs(y = "Log-response-ratio",
       x = NULL) +
  scale_x_reordered() +
  scale_fill_manual(values = c("red4", "dodgerblue")) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        strip.background = element_rect(fill = "tan"),
        strip.text = element_text(size = rel(0.5)))

ggsave("figs/pfig_logrr-all.png",
       width = 12, height = 12)
