# created 5 nov 2024
# purpose: do stats on fall biomass
# notes: It is not clear if in 2019 the volunteers were omitted, 
# -- or if they got lumped in another cat
# -- Bo says they got lumped in 'non crop' category
# -- must combine volunteer and weeds data into 'non-cover crop'


library(tidyverse)
library(lme4)
library(lmerTest)
library(CENTSdata)
library(broom)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(car)

library(multcomp)

rm(list = ls())

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
y <- as_tibble(cents_fallbio)
c <- 
  as_tibble(cents_cropyields) %>% 
  mutate(year = year(date2)) %>% 
  dplyr::select(year, crop)

w <- 
  read_csv("data/tidy_weaclass.csv") %>% 
  unite(col = "weayear", precip, te, sep  = "/") %>%
  left_join(c) %>% 
  mutate(weayear = paste(year, weayear, crop, sep = ", ")) %>% 
  dplyr::select(-crop)


#--data
d <- 
  eu %>% 
  left_join(y) %>% 
  mutate(year = year(date2)) %>% 
  left_join(w) %>% 
  dplyr::select(weayear, everything()) %>% 
  distinct()

# viz ---------------------------------------------------------------------

#--there are a fair number of zeros in the data
d %>% 
  ggplot(aes(dm_gm2)) +
  geom_histogram()

#--it is mostly from the weeds category in 2018
d %>% 
  ggplot(aes(dm_gm2)) +
  geom_histogram() +
  facet_grid(year~dm_type)

#--pretty evenly dist throughout the treatments
d %>% 
  ggplot(aes(dm_gm2)) +
  geom_histogram() +
  facet_grid(cctrt_id~dm_type)

d %>% 
  ggplot(aes(dm_type, dm_gm2)) +
  geom_point() +
  facet_grid(.~year) +
  labs(title = "Fall dry matter")


# combine weeds and volunteers --------------------------------------------

d1 <- 
  d %>% 
  mutate(dm_type2 = case_when(
    dm_type == "volunteer" ~ "other",
    dm_type == "weeds" ~ "other",
    dm_type == "grass_cl" ~ "covercrop",
    dm_type == "radish" ~ "covercrop",
    TRUE ~ dm_type)
    ) %>% 
  group_by(subplot_id, date2, year, dm_type2) %>% 
  summarise(dm_gm2 = sum(dm_gm2, na.rm = T)) %>% 
  left_join(eu) %>% 
  left_join(w) %>% 
  distinct()

#--they are not strongly related
d1 %>% 
  pivot_wider(names_from = dm_type2, values_from = dm_gm2)  %>% 
  left_join(eu) %>% 
  ggplot(aes(covercrop, other)) +
  geom_point(aes(color = cctrt_id))

library(patchwork)

#--radish-mid had higher biomass, always?
p1  <- 
  d1 %>% 
  group_by(weayear, till_id, straw_id, cctrt_id, dm_type2) %>% 
  summarise(dm_gm2 = mean(dm_gm2, na.rm  = T)) %>%
  filter(straw_id == "removed") %>% 
  ggplot(aes(till_id, dm_gm2)) +
  geom_col(aes(fill = dm_type2), color = "black") +
  facet_grid(weayear ~ straw_id + cctrt_id) +
  scale_fill_manual(values = c("gold", "darkblue"))

p2  <- 
  d1 %>% 
  group_by(weayear, till_id, straw_id, cctrt_id, dm_type2) %>% 
  summarise(dm_gm2 = mean(dm_gm2, na.rm  = T)) %>%
  filter(straw_id == "retained") %>% 
  ggplot(aes(till_id, dm_gm2)) +
  geom_col(aes(fill = dm_type2), color = "black") +
  facet_grid(weayear ~ straw_id + cctrt_id) +
  scale_fill_manual(values = c("gold", "darkblue"))


p1 + p2


#--removing straw had lowest impact, and no interactions
d1 %>% 
  group_by(weayear, till_id, cctrt_id, dm_type2) %>% 
  summarise(dm_gm2 = mean(dm_gm2, na.rm  = T)) %>%
  ggplot(aes(till_id, dm_gm2)) +
  geom_col(aes(fill = dm_type2), color = "black") +
  facet_grid(weayear ~ cctrt_id) +
  scale_fill_manual(values = c("gold", "darkblue"))


# model on total biomass-------------------------------------------------------------------

dtot <- 
  d1 %>% 
  group_by(subplot_id, date2, year, eu_id, block_id, plot_id, till_id, 
           rot_id, straw_id, cctrt_id, weayear) %>% 
  summarise(dm_gm2 = sum(dm_gm2, na.rm = T))

#--same model as for crop yields

m1 <- lmer(dm_gm2 ~ till_id * cctrt_id * straw_id * weayear + 
             (1|block_id),
           #(1|cctrt_id:till_id:straw_id) + 
           #(1|till_id:straw_id) +
           #(1|straw_id), 
           data = dtot)

summary(m1)
anova(m1)
plot(simulateResiduals(m1))
plot(m1)
#--this looks good, the pattern is fine
qqnorm(resid(m1))
qqline(resid(m1))

r <- tidy(anova(m1))

r %>% 
  mutate_at(c("sumsq", "meansq", "NumDF", "DenDF"), round) %>% 
  mutate(
    statistic = round(statistic, 2),
    p.value = round(p.value, 3),
         p.value = ifelse(p.value == 0, "<.001", p.value)) %>% 
  write_csv("data/stats_anovafallbio.csv")

r %>% 
  filter(p.value < 0.05)

r %>% 
  filter(p.value < 0.01)

#--year had strongest effect by far
#--effect of straw, no moderators 

#--effect of tillage, depending on year and cc if p= 0.05
#--effect of cc, depnding on tillage and year


# full model straw -------------------------------------------------------------------

emmeans(m1, specs = pairwise ~straw_id:weayear) 
emmeans(m1, specs = ~weayear) 

em_m1till <- emmeans(m1, specs = pairwise ~ till_id) 

#--compare no-till to other two tillages for each 
notill <- c(0, 0, 1)
others <- c(0.5, 0.5, 0)

contrast(em_m1till, method = list("notill - all" = notill - others) )


# full model tillage ------------------------------------------------------

a <- 
  tidy(emmeans(m1, specs = pairwise ~till_id:weayear)$contrasts) %>% 
  separate(contrast, into = c("v1", "v2"), sep = "-")

a18 <- 
  a %>% 
  filter(grepl("2018", v1) & grepl("2018", v2))

a19 <- 
  a %>% 
  filter(grepl("2019", v1) & grepl("2019", v2))


# full model till x cctrt -------------------------------------------------

txc <- 
  tidy(emmeans(m1, specs = pairwise ~till_id:cctrt_id)$contrasts) %>% 
  separate(contrast, into = c("v1", "v2"), sep = "-")

#--radish late
  txc %>% 
  filter(grepl("rad_L", v1) & grepl("rad_L", v2))

#--radish mid
txc %>% 
  filter(grepl("rad_M", v1) & grepl("rad_M", v2))


#--nocc vs radish mid
zzz<- 
  txc %>% 
  filter(grepl("nocc", v1) & grepl("rad_M", v2))

#--mix e
txc %>% 
  filter(grepl("mix_E", v1) & grepl("mix_E", v2))

#--mix m
txc %>% 
  filter(grepl("mix_M", v1) & grepl("mix_M", v2))

#--mix e
txc %>% 
  filter(grepl("mix_E", v1) & grepl("mix_E", v2))

#--nocc
zzz<-txc %>% 
  filter(grepl("nocc", v1) & grepl("nocc", v2))

#--notill
zzz <- txc %>% 
  filter(grepl("notill", v1) & grepl("notill", v2))

#--noninv
zzz <- txc %>% 
  filter(grepl("non", v1) & grepl("non", v2))


# full model cc ------------------------------------------------------

b <- 
  tidy(emmeans(m1, specs = pairwise ~cctrt_id:weayear)$contrasts) %>% 
  separate(contrast, into = c("v1", "v2"), sep = "-")

b18 <- 
  b %>% 
  filter(grepl("2018", v1) & grepl("2018", v2)) %>% 
  arrange(estimate)

b19 <- 
  b %>% 
  filter(grepl("2019", v1) & grepl("2019", v2))%>% 
  arrange(estimate)


# run separately for each year --------------------------------------------

m2018 <- lmer(dm_gm2 ~ till_id * cctrt_id * straw_id + 
             (1|block_id),
           #(1|cctrt_id:till_id:straw_id) + 
           #(1|till_id:straw_id) +
           #(1|straw_id), 
           data = dtot %>% filter(year == 2018))

m2019 <- lmer(dm_gm2 ~ till_id * cctrt_id * straw_id + 
                (1|block_id),
              #(1|cctrt_id:till_id:straw_id) + 
              #(1|till_id:straw_id) +
              #(1|straw_id), 
              data = dtot %>% filter(year == 2019))

#--no interactions
tidy(anova(m2018)) %>% 
  filter(p.value < 0.05)

#--no interactions
tidy(anova(m2019)) %>% 
  filter(p.value < 0.05)

emmeans(m2019, specs = pairwise ~ till_id) 
emmeans(m2019, specs = pairwise ~ till_id) 

# main effects -------------------------------------------------------------

ex <- emmeans(m2018, specs = ~ cctrt_id) 
cld(ex)

#--2018
ex2 <- emmeans(m2018, specs = ~ cctrt_id:till_id:straw_id, alpha = 0.01) 
res2 <- tidy(cld(ex2))


res2 %>% 
  arrange(estimate) %>% 
  mutate(group = as.factor(.group),
         trt = paste(cctrt_id, till_id, straw_id)) %>%
  ggplot(aes(trt, estimate)) +
  geom_point(aes(color = group), size = 5) +
  coord_flip() +
  facet_grid(till_id + straw_id ~ ., scales = "free_y")


#--2019
ex3 <- emmeans(m2019, specs = ~ cctrt_id:till_id:straw_id, alpha = 0.01) 
res3 <- tidy(cld(ex3))


res3 %>% 
  arrange(estimate) %>% 
  mutate(group = as.factor(.group),
         trt = paste(cctrt_id, till_id, straw_id)) %>%
  ggplot(aes(trt, estimate)) +
  geom_point(aes(color = group), size = 5) +
  coord_flip() +
  facet_grid(till_id + straw_id ~ ., scales = "free_y")


emmeans(m2018, specs = ~ cctrt_id) %>% 
  tidy() %>% arrange(estimate)

emmeans(m1, specs = ~ cctrt_id)

em_cc <- emmeans(m1, specs = pairwise ~ cctrt_id)
em_cc_means <- tidy(em_cc$emmeans) 

em_cc_means %>% 
  mutate(letter_sig = c("a", "a", "ab", "ab", "b")) %>% 
  ggplot(aes(cctrt_id, estimate)) +
  geom_errorbar(aes(x = cctrt_id, ymin = estimate - std.error, ymax = estimate + std.error),
                width = 0.2) +
  geom_point() +
  geom_text(aes(x = cctrt_id, y = estimate + 0.3, label = letter_sig)) +
  scale_y_continuous(limits = c(0, 4.5))

em_cc
radM <- c(0, 0, 0, 0, 1)
mixes <- c(0.5, 0.5, 0, 0, 0)
contrast(em_cc, method = list("radM - mixes" = radM - mixes))


# tillage by straw --------------------------------------------------------

em_ts <- emmeans(m2, specs = pairwise ~ till_id:straw_id)

em_ts$contrasts

#--when removing straw, no-till yields less than the other tillages
#--when retaining straw, no difference in yields

#--compare no-till to other two tillages for each 
notillnostraw <- c(0, 0, 1, 0, 0, 0)
othersnostraw <- c(0.5, 0.5, 0, 0, 0, 0)


contrast(em_ts, method = list("notill - all" = notill - othersnostraw) )

# emmeans for all (except crop)---------------------------------------------------------

em_all <- tidy(emmeans(m2, specs = pairwise ~ till_id|straw_id|cctrt_id)$emmeans) 

em_all %>% 
  ggplot(aes(straw_id, estimate)) +
  geom_errorbar(aes(x = straw_id, ymin = estimate - std.error, ymax = estimate + std.error, 
                    color = cctrt_id),
                width = 0.2) +
  geom_point(aes(color = cctrt_id)) +
  facet_grid(.~till_id) +
  scale_y_continuous(limits = c(0, 4.7))
