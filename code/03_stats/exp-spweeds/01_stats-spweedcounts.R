# created 17 feb 2025
# purpose: do stats on spring weed counts
# notes: 3 subreps
#   Q1: impact on total number of weeds
#   Q2: impact on total number of perennial weeds

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

draw <- 
  y %>% 
  mutate(year = year(date2),
         yearF = paste0("Y", year)) %>% 
  left_join(eu) %>% 
  left_join(w) %>% 
  mutate(weayear = paste(year, precip, sep = "-"),
         weed_type2 = ifelse(weed_type %in% c("dicot", "monocot"), "A", "P"))



# viz ---------------------------------------------------------------------

draw

#--there are lots of zeros!
draw %>% 
  ggplot(aes(count)) +
  geom_histogram()

draw %>% 
  ggplot(aes(count)) +
  geom_histogram(aes(fill = weed_type2))

#--do the number of weeds differ by trt?
draw %>% 
  group_by(weed_type2, cctrt_id, till_id, year) %>% 
  summarise(count = mean(count)) %>% 
  ggplot(aes(cctrt_id, count)) +
  geom_col(aes(fill = weed_type2)) +
  facet_grid(year~till_id) +
  labs(title = "Mean spring Weed Count") +
  coord_flip()


#--definitely over-dispersed
#--use negative binomial1 (linear) or nbinom2 (faster ince)
draw %>% 
  summarise(ave = mean(count),
            var = sd(count)^2)

#--need to sum up the sub-reps
d <- 
  draw %>% 
  group_by(year, yearF, weed_type, weed_type2, 
           block_id, plot_id, subplot_id, 
           straw_id, till_id, cctrt_id, 
           weayear) %>% 
  summarise(count = sum(count)) %>% 
  ungroup()


# 1. model on total weeds-------------------------------------------------------------------

dtot <- 
  d %>%  
  group_by(year, yearF, 
           block_id, plot_id, subplot_id, 
           straw_id, till_id, cctrt_id) %>% 
  summarise(count = sum(count)) %>% 
  ungroup()

dtot %>% 
  summary()

m_nb1 <- glmmTMB(count ~ yearF*till_id * straw_id * cctrt_id 
                    + (1 | block_id/till_id/cctrt_id),
                    ziformula = ~1,
                    family = nbinom1(), 
                    data = dtot)

sim_resnb1 <- simulateResiduals(m_nb1)
plot(sim_resnb1)

m_nb2 <- glmmTMB(count ~ yearF*till_id * straw_id * cctrt_id 
                 + (1 | block_id/till_id/cctrt_id),
                 ziformula = ~1,
                 family = nbinom2, 
                 data = dtot)

sim_resnb2 <- simulateResiduals(m_nb2)
plot(sim_resnb2)

AIC(m_nb1, m_nb2)

#--m1 is slightly better AIC
summary(m_nb1)
#--might not need an overdispersion parameter according to this

#--did not converge
m_p1 <- glmmTMB(count ~ yearF*till_id * straw_id * cctrt_id 
                 + (1 | block_id),
                 #ziformula = ~1,
                 family = poisson, 
                 data = dtot)

#--yeah this looks awful!
sim_resp1 <- simulateResiduals(m_p1)
plot(sim_resp1)

#--both negative binomials give similar answers
#--freaking everything is significant
Anova(m_nb1)
Anova(m_nb2)

m1 <- m_nb1

# 2. summarise total weeds model ---------------------------------------------------------

#--within inv, no diffs
#--within notill, mix_E diff from everything else
#--within surface, no diffs
emmeans(m1, pairwise ~cctrt_id|till_id)

#--year is an amplifier
emmip(m1, till_id ~ cctrt_id|yearF, type = "response")

#--tillage is largest driver
em2 <- emmeans(m1, ~till_id, type = "response")
pairs(em2)
1/0.501
#--inversion had 3 times more weeds than NT
#--surface had 2 times more weeds than NT

#--cctrt next largest
em2 <- emmeans(m1, ~cctrt_id|till_id, type = "response")
pairs(em2)
#--no impact of cc in inv or surf
#--within NT, mix E had more than twice as many weeds as other trts

#--straw? next largest
em3 <- emmeans(m1, ~cctrt_id|straw_id|till_id, type = "response")
pairs(em3)

#--keep emmeans for figure, over straw and year
res <- tidy(em2)

res %>% 
  write_csv("data/stats_emmeans/emmeans-spweedcounts.csv")


# 2. model on number of perenn weeds?-------------------------------------------------------------------

#summary: none of the models fit, proportion is prob better analys

d2 <- 
  d %>% 
  filter(weed_type2 == "P")

d2 %>% 
  ggplot(aes(count)) +
  geom_histogram()

#--problems
m2_nb1 <- glmmTMB(count ~ yearF*till_id * straw_id * cctrt_id 
                 + (1 | block_id/till_id),
                 ziformula = ~1,
                 family = nbinom1(), 
                 data = d2)

sim_resnb1 <- simulateResiduals(m2_nb1)
plot(sim_resnb1)

m2_nb2 <- glmmTMB(count ~ yearF*till_id * straw_id * cctrt_id 
                 + (1 | block_id/till_id),
                 ziformula = ~1,
                 family = nbinom2, 
                 data = d2)

sim_resnb2 <- simulateResiduals(m2_nb2)
plot(sim_resnb2)

#--did not converge
m2_p1 <- glmmTMB(count ~ yearF*till_id * straw_id * cctrt_id 
                + (1 | block_id),
                ziformula = ~1,
                family = poisson, 
                data = dtot)

#--yeah this looks awful!
sim_resp1 <- simulateResiduals(m2_p1)
plot(sim_resp1)

Anova(m2_nb1)
Anova(m2_nb2)

#--year is an amplifier
emmip(m_nb1, till_id ~ cctrt_id|yearF, type = "response")

#--tillage is largest driver
em1 <- emmeans(m_nb1, ~till_id, type = "response")
pairs(em1)
#--inversion had 3 times more weeds than NT
#--surface had 2 times more weeds than NT

#--cctrt next largest
em2 <- emmeans(m_nb1, ~cctrt_id|till_id, type = "response")
pairs(em2)
#--no impact of cc in inv or surf
#--within NT, mix E had more than twice as many weeds as other trts



# OLDDDDD -----------------------------------------------------------------


model <- glmmTMB(count ~ weed_type + till_id * straw_id * cctrt_id * weayear + 
                   (1 | block_id/till_id/cctrt_id),# + 
                   #(1 | weed_type), 
                 ziformula = ~ weed_type,  # Allow different zero-inflation per species
                 family = nbinom2, 
                 data = d)


# Check the model summary
#--doesn't work
summary(model)

#--simplify to only circ
d_cir <- 
  d %>% 
  filter(weed_type == "cirar") %>% 
  group_by(year, weed_type, weed_type2, 
           block_id, plot_id, subplot_id, 
           straw_id, till_id, cctrt_id, 
           weayear) %>% 
  summarise(count = sum(count)) %>% 
  select(count, everything())


d_cir %>% 
  ggplot(aes(plot_id, count)) +
  geom_point()

#--pretty equally dist betwn cc trts
d_cir %>% 
  ggplot(aes(count)) +
  geom_histogram(aes(fill = cctrt_id))

#--and till id
d_cir %>% 
  ggplot(aes(count)) +
  geom_histogram(aes(fill = till_id))

#--and straw id
d_cir %>% 
  ggplot(aes(count)) +
  geom_histogram(aes(fill = straw_id))

#--and year
d_cir %>% 
  ggplot(aes(count)) +
  geom_histogram(aes(fill = as.factor(year)))

#--but it is overdispered, use nbinom
#--doesn't converge
model_cir <- glmmTMB(count ~ till_id * cctrt_id * + 
                   (1 | till_id),
                 ziformula = ~ 1,  
                 family = poisson, 
                 data = d_cir)

plot(residuals(model_cir, type = "deviance"), main = "Deviance Residuals", ylab = "Residuals", xlab = "Index")

#--seems like I run out of df?
summary(model_cir)
Anova(model_cir, type = "III")
#--this gives the 'likelihood' of finding a circ in a thing
pairs(emmeans(model_cir, ~ cctrt_id | till_id, type = "response"))
plot(emmeans(model_cir, ~ cctrt_id * till_id))

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


# 2. log odds -------------------------------------------------------------

#chatgpt says if modelling presence/absence data, over dispersion is less of an issue

d1 <- 
  d %>% 
  group_by(year, weed_type2, block_id, plot_id, subplot_id, till_id, rot_id,
           cctrt_id) %>% 
  summarise(count = sum(count))
  select()

m1 <- glmer(WeedType ~ Year + CroppingSystem + (1|Plot), 
               data = your_data, 
               family = binomial)
summary(model)
