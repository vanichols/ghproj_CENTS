# created 6 nov 2024
# purpose: do stats on vegetation cover, community analysis
# notes: 3 subreps make up a single eu
#         based on code from my cover crop paper

library(tidyverse)
library(lme4)
library(lmerTest)
library(CENTSdata)
library(broom)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(car)
library(vegan)

library(writexl)


rm(list = ls())

# data --------------------------------------------------------------------

w <- cents_wea
eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_fallpctcover)

#--ohhh I'm treating each subrep separately. that's ok.
y1 <- 
  y %>% 
  mutate(
    year = lubridate::year(date2),
    eu = paste(eu_id, subrep, year, sep = "_")) 

#--data frame ready to made into a matrix
df_dat <- 
  y1 %>% 
  select(eu, eppo_code, cover_pct) %>%
  filter(eppo_code != "soil") %>% 
  mutate(cover_pct = round(cover_pct, 0)) %>% 
  pivot_wider(names_from = eppo_code, values_from = cover_pct) 

#--replace NAs with 0s
df_dat[is.na(df_dat)] <- 0

# analysis, no soil----------------------------------------------------------------

mat_dat <- 
  df_dat %>% 
  column_to_rownames(var = "eu")


#--evenness is shanndon div index / ln(richness)

## Species richness (S) and Pielou's evenness (J):
# S <- specnumber(BCI) ## rowSums(BCI > 0) does the same...
# J <- H/log(S)

res <- 
  mat_dat %>%
  mutate(shan = diversity(.),
         shan_hill= exp(shan),
         s = specnumber(.),
         evenness = shan/log(s))

d_res <-
  res %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  select(eu = rowname, shan, shan_hill, s, evenness)

#--is evenness = 1 when s = 1?
d_res |> 
  filter(evenness == 1)

# models -----------------------------------------------------------------

d_mod <- 
  d_res %>% 
  separate(eu, into = c("eu_id", "subrep", "year"), sep = "_") %>% 
  mutate(across(all_of(c("eu_id", "subrep", "year")), as.numeric)) %>% 
  left_join(cents_eukey) %>% 
  mutate(yearF = as.factor(year))

d_mod |> 
  ggplot(aes(yearF, evenness)) +
  geom_point()

d_mod |> 
  filter(evenness == 1)

# glmmTMB ---------------------------------------------------------

# If some values are exactly 0 or 1, adjust them slightly
# (Beta distribution requires data strictly within (0, 1))
eps <- 1e-4
d_mod$evenness_adj <- pmin(pmax(d_mod$evenness, eps), 1 - eps)

# Fit the model
m2 <- glmmTMB(
  evenness_adj ~ till_id * cctrt_id * straw_id * yearF +
    (1|block_id/straw_id/till_id/cctrt_id),
  family = beta_family(link = "logit"), # beta distribution, logit link
  data = d_mod
)

summary(m2)
sim_rest2 <- simulateResiduals(m2)
#--doesn't look great
plot(sim_rest2)


m2a <- glmmTMB(evenness_adj ~ till_id * cctrt_id * straw_id * yearF +
                (1|block_id/straw_id/till_id/cctrt_id),
              family=ordbeta(link = "logit"), 
              data= d_mod)

summary(m2a)
sim_rest2a <- simulateResiduals(m2a)
#--doesn't look great
plot(sim_rest2a)

#--they are very slightly different
anova(m2, m2a)


m0 <- m2
Anova(m0)

joint_tests(m0) |> 
  as_tibble() |> 
  write_xlsx("data/stats/supp_tables/evenness-anova.xlsx")


# inference ---------------------------------------------------------------


#--cctrt and year

emmeans(m0, specs = ~ cctrt_id|yearF, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(reorder(cctrt_id, response), response, color = cctrt_id)) +
  geom_point() +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  facet_grid(.~yearF) 

emmeans(m0, pairwise ~ cctrt_id|yearF)$contrasts %>% 
  as_tibble() %>% 
  write_xlsx("data/stats/supp_tables/evenness-ccyear-pairs.xlsx")

emmeans(m0, specs = ~ cctrt_id, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(reorder(cctrt_id, response), response, color = cctrt_id)) +
  geom_point() +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL))

emmeans(m0, ~ cctrt_id) %>% 
  as_tibble() %>% 
  write_xlsx("data/stats/supp_tables/evenness-cc-estimates.xlsx")

#--straw and year
#--radM evenness was higher when starw was retained
emmeans(m0, specs = ~ cctrt_id|straw_id, type = "response") |> 
  as_tibble() |> 
  ggplot(aes(reorder(cctrt_id, response), response, color = cctrt_id)) +
  geom_point() +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  facet_grid(.~straw_id) 

#--these are not meaningful differences
emmeans(m0, pairwise ~ straw_id|cctrt_id, type = "response")$contrasts %>% 
  tidy(.)

emmeans(m0, specs = ~ cctrt_id|straw_id, type = "response") |> 
  as_tibble() |> 
  write_xlsx("data/stats/supp_tables/evenness-ccstraw-estimates.xlsx")
