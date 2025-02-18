# created 17 feb 2025
# purpose: do stats on spring weed counts
# notes: 3 subreps

library(tidyverse)
library(lme4)
library(lmerTest)
library(CENTSdata)
library(broom)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(car)

rm(list = ls())

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_spweedcount)
w <- read_csv("data/tidy_weaclass.csv")

d <- 
  y %>% 
  mutate(year = year(date2)) %>% 
  left_join(eu) %>% 
  left_join(w) %>% 
  mutate(weayear = paste(year, precip, sep = "-"))

# viz ---------------------------------------------------------------------

d

#--there are lots of zeros!
d %>% 
  ggplot(aes(count)) +
  geom_histogram()

# model -------------------------------------------------------------------

# For the 4-way proportions for cover, you could actually analyse this a little di􀆯erently to what we talked about
# as well. You could pivot your data into long-format that has a column for the proportions, and another column
# for the Cover category (Soil, Weeds, Volunteers and CC). If you then analyse the proportions with a model like
# this:
  # model <- glmmTMB(proportion ~ CoverCategory * Tillage * CC * Straw * Year +
  #                    (1|Block) + (1|CC:Tillage:Straw) + (1|Tillage:Straw) + (1|Straw),
  #                  family=binomial(link=”logit”), data=DATA)
# You get an estimate of how di􀆯erent the proportions of each cover category are, and you are checking whether
# that is dependent on tillage, CC, straw or year e􀆯ects.

d

#--I am super confused which family (etc.) to use
#--this is a 'fractional logit model'?
m_bilogit <- glmmTMB(count ~ weed_type * till_id * cctrt_id * straw_id * weayear +
                   (1|block_id) + 
                   (1|cctrt_id:till_id:straw_id), #+ 
                   #(1|till_id:straw_id) + 
                   #(1|straw_id),
                 family=nbinom1, 
              data=d)

# Anova(m_bilogit)
# m_bilogit_simres <- simulateResiduals(m_bilogit)
# plot(m_bilogit_simres)

#--trying suggestion for a zero-inflated model
#--zero inflation applied to all equally (?)
m_bilogit_zinf <- update(m_bilogit, ziformula = ~1)
m_bilogit_zinfc <- update(m_bilogit, ziformula = ~weed_type)

anova(m_bilogit, m_bilogit_zinf, m_bilogit_zinfc)


zi_nb_model <- glmmTMB(y ~ x + (1 | group), 
                       ziformula = ~ 1, 
                       family = nbinom1, 
                       data = data)



# Install and load required package
if (!requireNamespace("glmmTMB", quietly = TRUE)) install.packages("glmmTMB")
library(glmmTMB)

# Simulated dataset
set.seed(123)
n <- 200  # Number of observations
group <- factor(rep(1:20, each = 10))  # Random effect (e.g., site, plot)
x <- rnorm(n)  # Continuous predictor
z <- rbinom(n, 1, 0.3)  # Zero inflation process

# Generate count data using a Poisson model with zero inflation
lambda <- exp(1 + 0.5 * x)  # Poisson mean
y <- rpois(n, lambda) * (1 - z)  # Apply zero inflation

# Combine into a dataframe
data <- data.frame(y, x, group)

# Fit a zero-inflated GLMM using a Poisson distribution
zi_model <- glmmTMB(y ~ x + (1 | group), 
                    ziformula = ~ 1,  # Zero-inflation model (intercept only)
                    family = poisson,
                    data = data)

# Model summary
summary(zi_model)

# Optional: Fit with negative binomial distribution if overdispersion is suspected
zi_nb_model <- glmmTMB(y ~ x + (1 | group), 
                       ziformula = ~ 1, 
                       family = nbinom1, 
                       data = data)

summary(zi_nb_model)
