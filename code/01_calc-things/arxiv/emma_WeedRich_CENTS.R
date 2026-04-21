# Read in packages ----
library(readxl)
library(tidyverse)
library(ggthemes)
library(ggridges)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(multcompView)
library(multcomp)

# Read in data ----

cover <- read.csv("centsdata/cents_fallpctcover.csv")
#biomass <- read.csv("centsdata/cents_fallbio.csv")

summary(cover)

View(cover)

### Prepare data ----

# Attach treatment info

# Read in the treatment data:
eukey <- read.csv("centsdata/cents_eukey.csv")

# Join to weed cover
cover <- cover %>% 
  left_join(eukey, join_by(eu_id == eu_id)) %>% 
  dplyr::select(!c("plot_id", "subplot_id", "rot_id"))

# change variable classes
cover <- cover %>% 
  mutate(across(c(subrep, cover_cat, eppo_code, block_id, till_id, straw_id, cctrt_id), factor),
         date2 = as.factor(cover$date2))
cover$date2 <- dplyr::recode(cover$date2, "2018-11-09" = '2018',
                             "2019-11-01" = '2019')

# Add diversity variables ----

# Calculate presence of weed species [1:0], soil should be NA, so should crop plants!! 
# Assuming that the following are the crops: radish (rapsr), ryegrass (lolpe), clover (trfre) oat (avesa), barley (horvw)
# NB: "other" in category also has the primary crops
distinct(cover, cover_cat, eppo_code)
# = only 10 possible weed species 


# Species pressence, filtering out soil and non-weed observations
cover_cropless <- cover %>% 
  mutate(spc_pres = if_else(cover_pct > 1, 1, 0),
         # pc_pres = if_else(eppo_code == "soil", NA, spc_pres),
         # cover_pct_no_soil = if_else(cover_cat == "soil", NA, cover_pct)
  ) %>% 
  filter(!(eppo_code %in% c("soil", "rapsr", "lolpe", "trfre", "avesa", "horvw")))

# Total cover in each sampled unit (that is: for each treatment combo, replicate and year, 3 spots are sampled = 3*2*5*2*4*3 = 720 sampled units)
cover_tot_cropless <- cover_cropless %>% 
  group_by(eu_id, straw_id, till_id, cctrt_id, block_id, date2, subrep) %>% 
  summarise(cov_tot = sum(cover_pct, na.rm = TRUE))

# join the total cover to the cover data
cover_weeds <- cover_cropless %>%
  left_join(cover_tot_cropless, join_by(eu_id, straw_id, till_id, cctrt_id, block_id, date2, subrep)) %>% 
  relocate(eu_id, straw_id, till_id, cctrt_id, block_id, date2, subrep, cov_tot, eppo_code, cover_pct, spc_pres)
### NB many sampling units DO NOT have any plant cover - especially when crops/volunteers are removed

#### Calculate weed species richness for each sampled quadrat inside each sub-sub-plot ----
## species richness as a count, shannon diversity, and shannon evenness
weed_div <- cover_weeds %>% 
  group_by(date2, block_id, eu_id, straw_id, till_id, cctrt_id, subrep) %>% 
  summarise(
    spec_rich = sum(spc_pres),
    poss_spec = n() # no. of species they looked for
  )

#How many non-zero values are there for each cover crop
weed_div %>% 
  filter(spec_rich > 0 & cctrt_id == "rad_M")
weed_div %>% 
  filter(spec_rich > 0 & cctrt_id == "rad_L")
weed_div %>% 
  filter(spec_rich > 0 & cctrt_id == "mix_E")
weed_div %>% 
  filter(spec_rich > 0 & cctrt_id == "mix_M")
weed_div %>% 
  filter(spec_rich > 0 & cctrt_id == "nocc")

#
##### Visualize:  Question: How is weed species richness affected by straw retention, tillage and cover crop? ####
# scatterplot
weed_div %>% 
  ggplot(aes(x = reorder(cctrt_id, spec_rich), y = spec_rich, colour = date2, shape = block_id)) +
  geom_point(position = "jitter") + # 
  facet_grid(straw_id ~ till_id) +
  labs(x = "cover crop treatment",
       y = "species richness (no.)",
       color = "year")

# commulative probability 
weed_div %>% 
  ggplot(aes(x = spec_rich, color = cctrt_id)) +
  geom_density(linewidth = 1, alpha = 0.4, stat = "ecdf") + #, stat = "ecdf") + 
  facet_grid(straw_id ~ till_id) +
  labs(x = "species richness (no.)",
       #y = "",
       #color = "Cover crop treatment"
  ) #+
#scale_color_colorblind()
#scale_fill_colorblind()

# Density ridges
weed_div %>% 
  ggplot(aes(x = spec_rich, y = cctrt_id, fill = cctrt_id, color = cctrt_id)) +
  geom_density_ridges(alpha = 0.4) + #, stat = "ecdf") + 
  facet_grid(straw_id ~ till_id) +
  labs(x = "species richness (no.)",
       #y = "",
       #color = "Cover crop treatment"
  ) +
  scale_color_colorblind() +
  scale_fill_colorblind()

# Boxplot
weed_div %>% 
  ggplot(aes(x = cctrt_id, y = spec_rich, fill = cctrt_id)) + #, color = cctrt_id)) +
  geom_boxplot() + #, stat = "ecdf") + 
  facet_grid(straw_id ~ till_id) +
  labs(x = "species richness (no.)",
       #y = "",
       #color = "Cover crop treatment"
  ) 

#
###
# Now try to fit a model ----
### 
#

# Weed species presence is count data, i could use poisson, but the qqplot is not great - the negative binomial makes sense, also because i have so many zero responses
weed_mod1 <- glmmTMB(spec_rich ~ straw_id + till_id + cctrt_id + 
                       till_id:cctrt_id + straw_id:cctrt_id + straw_id:till_id +  # 
                       (1| block_id/straw_id/till_id/cctrt_id/date2),
                     family = nbinom2(link="log"),
                     data = weed_div)

dia_mod1 <- plot(simulateResiduals(weed_mod1))
dia_mod1

# Look at the predictions with scatterplot:
weed_div$fits1 <- fitted(weed_mod1)

weed_plot1 <- weed_div %>% 
  ggplot(aes(x = cctrt_id, y = spec_rich, colour = cctrt_id, shape = date2)) +
  geom_point(position = "jitter") + # observed points; position = "jitter"
  geom_point(aes(y = fits1), color = "black") + # predicted points
  # geom_segment(aes(x = ccdm_gm2, xend = ccdm_gm2, y = spec_rich, yend = fits), color = "red") +
  facet_grid(straw_id ~ till_id) +
  labs(
    x = "Cover crop treatment",
    y = "species richness 
    (number of weed species)",
    color = "block id",
    shape = "year"
  ) 
weed_plot1

# Cumulative probability step
weed_plot2 <- weed_div %>% 
  ggplot(aes(x = spec_rich, color = cctrt_id)) +
  geom_step(alpha = 0.8, stat = "ecdf") + #, stat = "ecdf") + linewidth = 1, 
  geom_step(aes(x = fits1), alpha = 0.8, linetype = 2, stat = "ecdf") + #, linewidth = 1
  facet_grid(straw_id ~ till_id, scales = "free") +
  scale_color_discrete() + #labels = c("Grass & clover (early)", "Grass & clover (mid)", "No cc", "Radish (late)", "Radish (mid)")) +
  #scale_color_colorblind() +
  labs(title = "Weed species richness depending on straw-, tillage- and 
       cover crop management",
       x = "Species richness (count)",
       y = "Cumulative distribution",
       color = "Cover crop treatment"
  ) +
  theme_bw()

weed_plot2
ggsave("weed_cumprob_model_step.png", path = "output", weed_plot2, width=16, height=10, units="cm")

# Cumulative probability density
weed_plot3 <- weed_div %>% 
  ggplot(aes(x = spec_rich, color = cctrt_id)) +
  geom_density(stat = "ecdf") + #, stat = "ecdf") + linewidth = 1, 
  geom_density(aes(x = fits1), linetype = 2, stat = "ecdf") + #, linewidth = 1
  facet_grid(straw_id ~ till_id) +
  scale_color_discrete() + #labels = c("Grass & clover (early)", "Grass & clover (mid)", "No cc", "Radish (late)", "Radish (mid)")) +
  #scale_color_colorblind() +
  labs(title = "Weed species richness depending on straw-, tillage- and 
       cover crop management",
       x = "Species richness (count)",
       y = "Cumulative distribution",
       color = "Cover crop treatment"
  ) +
  theme_bw()
weed_plot3

# boxplot
weed_plot4 <- weed_div %>% 
  ggplot(aes(x = cctrt_id, y = spec_rich, color = cctrt_id)) +
  geom_boxplot() + #, stat = "ecdf") + linewidth = 1, 
  geom_boxplot(aes(y = fits1), color = "black", linetype = 1) + #, linewidth = 1
  facet_grid(straw_id ~ till_id) +
  scale_color_discrete(labels = c("Grass & clover (early)", "Grass & clover (mid)", "No cc", "Radish (late)", "Radish (mid)")) +
  theme_bw()
weed_plot4

# Density ridges
weed_plot5 <- weed_div %>% 
  ggplot(aes(x = spec_rich, y = cctrt_id, color = cctrt_id)) +
  geom_density_ridges(aes(fill = cctrt_id), alpha = 0.4) +
  geom_density_ridges(aes(x = fits1), linetype = 2, linewidth = 0.7, alpha = 0.001) +
  facet_grid(straw_id ~ till_id) +
  labs(x = "species richness (no.)",
       y = "cover crop treatment",
       color = "Cover crop treatment",
       fill = "Cover crop treatment",
  ) +
  theme_bw()# 
# Maybe not so informative for count data!
weed_plot5

# 
##
#---- Evaluate!! ----#
##
#
car::Anova(weed_mod1)
summary(weed_mod1)

head(model.matrix(weed_mod1))

# pairwise comparison of estimated marginal means
weed_emm_cctill <- emmeans(weed_mod1, 
                           pairwise~ cctrt_id | till_id,
                           type = "response")
weed_emm_cctill

weed_emm_cctill_plot <- plot(weed_emm_cctill)
weed_emm_cctill_plot
ggsave("weed_emm_cctill.png", weed_emm_cctill_plot, path = "output", width=10, height=7, units="cm")

weed_emm_tillcc <- emmeans(weed_mod1, 
                           pairwise~ till_id | cctrt_id,
                           type = "response")
weed_emm_tillcc
plot(weed_emm_tillcc)

weed_emm_straw <- emmeans(weed_mod1, 
                          pairwise ~ straw_id,
                          type = "response")
weed_emm_straw