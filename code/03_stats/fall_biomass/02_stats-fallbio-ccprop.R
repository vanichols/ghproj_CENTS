# created 19 dec 2024
# purpose: do stats on fall biomass proportion cover crop
# notes: not sure how to do proportions correctly
Sys.setenv(LANG = "en")

library(tidyverse)
library(lme4)
library(lmerTest)
library(CENTSdata)
library(broom)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(car)
library(patchwork)

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


# calculate proportion cc -------------------------------------------------

d2 <- 
  d1 %>% 
  pivot_wider(names_from = dm_type2, values_from = dm_gm2) %>% 
  mutate(cc_prop = covercrop / (covercrop + other),
         other_prop = 1 - cc_prop)
  

#--radish-mid had highest proportion that was cover crop
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


#--did removing straw impact proportion that was cc?
p1 + p2

d2 %>% 
  ggplot(aes(till_id, other_prop)) +
  geom_point(aes(color = straw_id)) +
  facet_grid(weayear ~ cctrt_id) 


#--removing straw had lowest impact, and no interactions
d1 %>% 
  group_by(weayear, till_id, cctrt_id, dm_type2) %>% 
  summarise(dm_gm2 = mean(dm_gm2, na.rm  = T)) %>%
  ggplot(aes(till_id, dm_gm2)) +
  geom_col(aes(fill = dm_type2), color = "black") +
  facet_grid(weayear ~ cctrt_id) +
  scale_fill_manual(values = c("gold", "darkblue"))


# model on total biomass-------------------------------------------------------------------

#--same model as for fallpctcover
m1 <- glmmTMB(other_prop ~ till_id * cctrt_id * straw_id * weayear +
                (1|block_id) + 
                (1|cctrt_id:till_id:straw_id) + 
                (1|till_id:straw_id) + 
                (1|straw_id),
              family=binomial(link="logit"), 
              data=d2)

m2 <- glmmTMB(other_prop ~ till_id * cctrt_id * straw_id * weayear +
                (1|block_id),# + 
                #(1|cctrt_id:till_id:straw_id) + 
                #(1|till_id:straw_id) + 
                #(1|straw_id),
              family=binomial(link="logit"), 
              data=d2)

m3 <- glmmTMB(cc_prop ~ till_id * cctrt_id * straw_id * weayear +
                (1|block_id),# + 
              #(1|cctrt_id:till_id:straw_id) + 
              #(1|till_id:straw_id) + 
              #(1|straw_id),
              family=binomial(link="logit"), 
              data=d2)

#--yield model for reference
# m1 <- lmer(dm_gm2 ~ till_id * cctrt_id * straw_id * weayear + 
#              (1|block_id),
#            #(1|cctrt_id:till_id:straw_id) + 
#            #(1|till_id:straw_id) +
#            #(1|straw_id), 
#            data = dtot)

car::Anova(m2)
car::Anova(m3)

plot(simulateResiduals(m2))
plot(m2)
#--this doesn't look good
qqnorm(resid(m2))
qqline(resid(m2))

qqnorm(resid(m3))
qqline(resid(m3))

#--cover crop trt is only sig thing impacting other_prop
r <- tidy(Anova(m2))

r %>% 
  mutate(
    statistic = round(statistic, 2),
    p.value = round(p.value, 3),
         p.value = ifelse(p.value == 0, "<.001", p.value)) %>% 
  write_csv("data/stats_anovafallbioprop.csv")

emmeans(m3, specs = ~ cctrt_id:weayear:till_id, type = "response") %>% 
  tidy(.) %>% 
  ggplot(aes(reorder(till_id, prob), prob)) +
  geom_point(aes(color = weayear), size = 3) +
  facet_grid(.~cctrt_id)
