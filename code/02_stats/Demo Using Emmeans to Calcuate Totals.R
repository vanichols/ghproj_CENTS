library(tidyverse)
library(tweedie)
library(glmmTMB)
library(emmeans)

# Create super simple simulation of a study with only 1 factor and 2 weed types
# in a completely randomized design
trt_dict <- data.frame(Straw = c('Kept', 'Kept', 'Remove', 'Remove'),
                       Weed_Type = c('Annual', 'Perennial', 'Annual', 'Perennial'), 
                       Mean_Density = c(5, 2.5, 10, 3))
sim <- expand.grid(Straw = c('Kept', 'Remove'),
                   Weed_Type = c('Annual', 'Perennial'),
                   Rep = factor(1:4)) |> 
  left_join(trt_dict)

# In the tweedie distribution, power parameter should be between 1 and 2 (at
# either of those extremes, the tweedie reduces to either gamma or poisson
# distributions). Play around with mean, dispersion (= phi) and power values to
# get a feel for the distributions
hist(rtweedie(n = 1e4, mu = 1, phi = 1, power = 1.5))

# Simulating response values
sim$Density = sapply(sim$Mean_Density, FUN = rtweedie, n = 1, xi = NULL, phi = 1, power = 1.5)

# Fit very simple model
mod <- glmmTMB(Density ~ Straw*Weed_Type, data = sim, 
               family = tweedie)

# generate inverse-linked (i.e. back-transformed) estimates for all combinations
# of straw treatment and weed type:
(emm <- emmeans(mod, ~ Straw*Weed_Type, type = 'response'))

# Now create a list for the estimates we want, which are the sum of annual and
# perennial for the "kept" treatment and likewise for the "Removed", so the 
# following weight vectors (in the order corresponding to the output from 
# emmeans) is used:
est_list = list('Total Kept' = c(1, 0, 1, 0), 
                'Total Removed' = c(0, 1, 0, 1))

# Now we call "contrasts", after back-transforming using regrid(), and feeding
# in our list of estimates, while turning off the actual testing against zero
# (we don't care about that p-value) and turn of the generation of confidence
# intervals:
contrast(regrid(emm), est_list, infer = c(T, F))

# Compare with values from emmeans output, and we can see that the estimates
# check out, so it passes the sanity check.
