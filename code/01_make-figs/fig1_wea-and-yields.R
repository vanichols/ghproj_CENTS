library(CENTSdata)
library(patchwork)
library(tidyverse)
library(ggbeeswarm)

rm(list = ls())


source("code/00_color-palettes.R")

w <- cents_wea
cents_cropyields

dk <- 
  read_csv("data/tidy_dk-avg-yields.csv") %>% 
  mutate(crop = c("2018 (Spring barley)", "2019 (Spring oat)", "2020 (Faba bean)"))

# plot1 precip ------------------------------------------------------------------

w2 <- 
  w %>% 
  group_by(year) %>% 
  mutate(cprec_mm = cumsum(prec_mm),
         dprec_mm = cprec_mm - LTcprec_mm) %>% 
  ungroup() %>% 
  mutate(yearH = case_when(
    year == 2018 ~ "2018 (Spring barley)", 
    year == 2019 ~ "2019 (Spring oat)",
    year == 2020 ~ "2020 (Faba bean)", 
    TRUE ~ "1990-2020"
  )) %>% 
  mutate(cprecip = round(max(LTcprec_mm))) 

p1 <-
  w2 %>% 
  mutate(facet = "Cumulative Precipitation") %>% 
  filter(doy < 364) %>% 
  ggplot() +
  geom_line(aes(doy, dprec_mm, group = year, color = yearH)) +
  geom_hline(yintercept = 0) +
  geom_line(data = . %>% filter(year %in% c(2018, 2019, 2020)), 
            aes(doy, dprec_mm, group = year, color = yearH), linewidth = 1.2) +
  scale_color_manual(values = c("gray80", ylw1, dkbl1, c2)) +
  theme_bw() + 
 th1 +
  labs(x = "Day of year",
       y = "Deviation from\nLT mean (mm)", 
       color = NULL) +
  facet_grid(.~facet) +
  theme(legend.direction = "horizontal")

p1

# plot 2 (temperature) ------------------------------------------------------------------

t3 <- 
  w %>% 
  group_by(year) %>% 
  mutate(dt_c = avgte - LTavgte,
         cdt_c = cumsum(dt_c)) %>% 
  ungroup() %>% 
  mutate(yearH = case_when(
    year == 2018 ~ "2018 (Spring barley)", 
    year == 2019 ~ "2019 (Spring oat)",
    year == 2020 ~ "2020 (Faba bean)", 
    TRUE ~ "1990-2020"
  )) %>% 
  mutate(meanT = round(mean(LTavgte))) 



p2 <- 
  t3 %>% 
  mutate(facet = "Air Temperature") %>% 
  filter(doy < 364) %>% 
  ggplot() +
  geom_line(aes(doy, cdt_c, group = year, color = yearH)) +
  geom_hline(yintercept = 0) +
  geom_line(data = . %>% filter(year %in% c(2018, 2019, 2020)), 
            aes(doy, cdt_c, group = year, color = yearH), linewidth = 1.2) +
  scale_color_manual(values = c("gray80", ylw1, dkbl1, c2)) +
  theme_bw() + 
  th1 +
  labs(x = "Day of year",
       y = "Deviation from\nLT mean (deg C - days)", 
       color = NULL) +
  facet_grid(.~facet)+
  theme(legend.direction = "horizontal")


p2

# plot 3 square plot -------------------------------------------------------------


LTprec <-
  w2 %>% 
  select(cprecip) %>% 
  distinct() %>% 
  pull()

LTtemp <-
  t3 %>% 
  select(meanT) %>% 
  distinct() %>% 
  pull()

d3 <- 
  w2 %>% 
  select(doy, year, cprecip, cprec_mm) %>% 
  group_by(year) %>% 
  filter(cprec_mm == max(cprec_mm)) %>% 
  left_join(
    t3 %>% 
      select(doy, year, meanT, avgte) %>% 
      group_by(year, meanT) %>% 
      summarise(avgT = mean(avgte))
  ) %>% 
  mutate(yearH = case_when(
    year == 2018 ~ "2018 (Spring barley)", 
    year == 2019 ~ "2019 (Spring oat)",
    year == 2020 ~ "2020 (Faba bean)", 
    TRUE ~ "1990-2020"
  )) %>% 
  mutate(fakefacet = "Weather Summary")

p3 <- 
  ggplot() + 
  geom_point(data = d3 %>% filter(yearH == "1990-2020"),
             aes(x = cprec_mm, y = avgT), 
             size = 4, color = "gray80") +
  geom_point(data = d3 %>% filter(yearH == "2018 (Spring barley)"),
             aes(x = cprec_mm, y = avgT), 
             size = 4, color = ylw1, shape = 15) +
  geom_point(data = d3 %>% filter(yearH == "2019 (Spring oat)"),
             aes(x = cprec_mm, y = avgT), 
             size = 4, color = dkbl1, shape = 17) +
  geom_point(data = d3 %>% filter(yearH == "2020 (Faba bean)"),
             aes(x = cprec_mm, y = avgT), 
             size = 4, color = c2, , shape = 19) +
  geom_vline(xintercept = LTprec) +
  geom_hline(yintercept = LTtemp) +
  geom_text(aes(x = 700, y = 7, label = "Wet and cool"), 
            check_overlap = T, hjust = 0.5, fontface = "italic") +
  geom_text(aes(x = 700, y = 11, label = "Wet and hot"), 
            check_overlap = T, hjust = 0.5, fontface = "italic") +
  geom_text(aes(x = 475, y = 7, label = "Dry and cool"), 
            check_overlap = T, hjust = 0.5, fontface = "italic") +
  geom_text(aes(x = 475, y = 11, label = "Dry and hot"), 
            check_overlap = T, hjust = 0.5, fontface = "italic") +
  scale_color_manual(values = c("gray80", ylw1, dkbl1)) +
  scale_x_continuous(limits = c(400, 800)) +
  scale_y_continuous(limits = c(7, 11)) +
  theme_bw() +
  th1 +
  labs(x = "Mean cumulative precipitation\n(mm)",
       y = "Mean air temperatue\n(deg C)",
       color = NULL)+
  facet_grid(.~fakefacet)

p3


# plot 4 yields ------------------------------------------------------------------


d <- 
  cents_eukey %>% 
  left_join(cents_cropyields) %>% 
  mutate(estimate = yield_dry_Mgha) %>% 
  mutate(cctrt_nice = case_when(
    cctrt_id == "nocc" ~ "NoCC",
    cctrt_id == "mix_E" ~ "MixE",
    cctrt_id == "mix_M" ~ "MixM",
    cctrt_id == "rad_M" ~ "RadM",
    cctrt_id == "rad_L" ~ "RadL",
    TRUE~"XXX"
  ),
  cctrt_id = factor(cctrt_id, levels = ord.cctrt_id),
  cctrt_nice = factor(cctrt_nice, levels = ord.cctrt_niceS),
  crop = case_when(
    crop == "faba bean" ~ "2020 (Faba bean)",
    crop == "oat" ~ "2019 (Spring oat)",
    crop == "spring barley" ~ "2018 (Spring barley)"
  ),
  crop = factor(crop, levels = ord.crop))

# p4 <- 
#   d %>% 
#   ggplot(aes(cctrt_nice, yield_dry_Mgha)) +
#   geom_jitter(aes(color = crop, shape = crop), 
#               width = 0.1, alpha = 1, show.legend = F,
#               size = 1) +
#   geom_text(aes(x = "RadM", y = 0, label = "*")) +
#   scale_y_continuous(limits = c(0, 6)) +
#   scale_shape_manual(values = c(15, 17, 19)) +
#   scale_color_manual(values = c(ylw1, dkbl1, c2)) +
#   theme_bw() +
#   facet_grid(.~crop) +
#   labs(x = NULL,
#        y = "Dry grain yield<br>(Mg ha<sup>-1</sup>)",
#        fill = NULL,
#        shape = NULL) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         strip.background = element_rect(fill = "transparent"),
#         axis.title.y = element_markdown())

p4 <- 
  d %>% 
  ggplot(aes(cctrt_nice, yield_dry_Mgha)) +
  geom_boxplot(aes(fill = crop), show.legend = F) +
  # geom_jitter(aes(color = crop, shape = crop), 
  #               width = 0.1, size = 2) +
  # geom_beeswarm(aes(color = crop, shape = crop), 
  #               show.legend = F, cex = 2, size = 2) +
  geom_hline(data = dk,
             aes(yintercept = yield), linetype = "dashed") +
  geom_text(aes(x = "RadM", y = 0, label = "*"), size = 10) +
  scale_y_continuous(limits = c(0, 6)) +
  scale_shape_manual(values = c(15, 17, 19)) +
  scale_color_manual(values = c(ylw1, dkbl1, c2)) +
  scale_fill_manual(values = c(ylw1, dkbl1, c2)) +
  theme_bw() +
  facet_grid(.~crop) +
  labs(x = NULL,
       y = "Dry grain yield<br>(Mg ha<sup>-1</sup>)",
       fill = NULL,
       shape = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "transparent"),
        axis.title.y = element_markdown())

p4

# put together ------------------------------------------------------------


(p3 + p2 + p1) / p4 + plot_layout(guides = 'collect',  
                                  heights = c(1, 1.5)) & theme(legend.position = 'top')

ggsave("figs/fig1_wea-and-yields.png", width = 9, height = 7)


