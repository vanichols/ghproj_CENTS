#--fig showing eco values
#--created 9 oct 2025
#--for supplemental fig

library(tidytext)

source("code/00_color-palettes.R")

draw <- 
  read_csv("data/tidy_spvalue-raw.csv") 

d <- 
  draw %>% 
  filter(eco_service !="harm3") %>% 
  group_by(eco_service) %>% 
  mutate(maxval = max(raw_value),
         sc_value2 = raw_value/maxval)  %>% #--max value should be 1
  mutate(eco_cat = ifelse(eco_service %in% c("pol1", "pol2", "pol3"), 
                          "Pollinators", "Food web"))

#--rapsr is the best? always? not ALWAYS
d %>% 
  ungroup() %>% 
  mutate(eco_service = as.factor(eco_service),
         eppo_code = reorder_within(eppo_code, raw_value, eco_service)) %>% 
  ggplot(aes(eppo_code, raw_value)) +
  geom_point() +
  scale_x_reordered() +
  facet_wrap(~ eco_service, scales = "free") +
  coord_flip() +
  theme_bw()

#--this is ridiculous looking, good thing I scaled them within a category?
d %>% 
  ggplot(aes(reorder(eppo_code, raw_value, sum), raw_value)) +
  geom_col(aes(fill = eco_service), color = "black") +
  coord_flip()

#--scaled
d %>% 
  ggplot(aes(reorder(eppo_code, sc_value2, sum), sc_value2)) +
  geom_col(aes(fill = eco_service), color = "black") +
  coord_flip()

#--make it nice
d1 <- 
  d %>% 
  select(eco_cat, eco_service) %>% 
  distinct() %>% 
  group_by(eco_cat) %>% 
  mutate(eco_subcat = 1:n(), 
         eco_subcat = paste("Subcategory ", eco_subcat))

#--highlight the cover crop species
#--RAPSR, LOLPE, TRFRE
# Create custom labels: Bold only B and D
#--
custom_labels <- 
  d |> 
  group_by(eppo_code) |> 
  summarise(sc_value2 = sum(sc_value2)) |> 
  arrange(sc_value2) |> 
  mutate(eppo_code = str_to_upper(eppo_code)) |> 
  mutate(eppo_code = case_when(
    eppo_code == "RAPSR"~ "<b>RAPSR</b>",
    eppo_code == "LOLPE"~ "<b>LOLPE</b>",
    eppo_code == "TRFRE"~ "<b>TRFRE</b>",
    TRUE ~ eppo_code)) |> 
  pull(eppo_code) 


d %>% 
  left_join(d1) %>%
  mutate(eppo_code = str_to_upper(eppo_code)) %>% 
  ggplot(aes(reorder(eppo_code, sc_value2, sum), sc_value2)) +
  geom_col(aes(fill = eco_cat, alpha = eco_subcat), color = "black") +
  scale_x_discrete(labels = custom_labels) +
  scale_y_continuous(limits = c(0, 6)) +
  coord_flip() +
  scale_fill_manual(values = c(dkbl1, ylw1)) +
  labs(alpha = "Sub-service category",
       fill = "Ecological service category", 
       x = "EPPO code",
       y = "Scaled potential value (maximum value of 6)",
       title = "Cover crop species are bolded") +
  theme(legend.direction = "vertical", legend.box = "vertical",
        axis.text.x = element_markdown(size = 12, color = "black"),
        axis.text.y = element_markdown(size = 12, color = "black"))


ggsave("figs/sfig_species-eco-values.png",
       width = 6.8, 
       height = 4.7)
