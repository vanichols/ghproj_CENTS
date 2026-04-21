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


rm(list = ls())

# data --------------------------------------------------------------------

w <- cents_wea
eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_fallpctcover)

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


# models? -----------------------------------------------------------------

d_mod <- 
  d_res %>% 
  separate(eu, into = c("eu_id", "subrep", "year"), sep = "_") %>% 
  mutate(across(all_of(c("eu_id", "subrep", "year")), as.numeric)) %>% 
  left_join(cents_eukey) %>% 
  mutate(yearF = as.factor(year))


# lmer not right ----------------------------------------------------------

m1 <- lmer(evenness ~ cctrt_id * till_id * straw_id * yearF + (1|block_id),
           data = d_mod)

anova(m1)
emmeans(m1, specs = "cctrt_id")
emmeans(m1, pairwise ~ cctrt_id)$contrasts %>% 
  tidy(.) %>% 
  filter(adj.p.value < 0.05)

#--straw and cc itxn?
tst <- 
  emmeans(m1, pairwise ~ cctrt_id*straw_id)$contrasts %>% 
  tidy(.) %>% 
  filter(adj.p.value < 0.05)

d_mod %>% 
  ggplot(aes(cctrt_id, evenness)) +
  geom_boxplot() +
  facet_grid(.~year)

summary(d_mod$evenness)


# glmmTMB better? ---------------------------------------------------------
#--same results, don't worry about it too much

# If some values are exactly 0 or 1, adjust them slightly
# (Beta distribution requires data strictly within (0, 1))
eps <- 1e-4
d_mod$evenness_adj <- pmin(pmax(d_mod$evenness, eps), 1 - eps)

# Fit the model
m2 <- glmmTMB(
  evenness_adj ~ cctrt_id * till_id * straw_id * yearF + (1 | block_id),      # fixed + random effects
  family = beta_family(link = "logit"), # beta distribution, logit link
  data = d_mod
)

# Summarize results
summary(m2)

# Likelihood ratio test or Wald test for significance
Anova(m2)

emmeans(m2, pairwise ~ cctrt_id)$contrasts %>% 
  tidy(.) %>% 
  filter(adj.p.value < 0.05)
