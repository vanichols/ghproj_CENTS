library(CENTSdata)
library(patchwork)
library(tidyverse)

source("code/00_color-palettes.R")


# 1. weather plots -----------------------------------------------------------

w <- cents_wea

w2 <- 
  w %>% 
  group_by(year) %>% 
  mutate(cprec_mm = cumsum(prec_mm),
         dprec_mm = cprec_mm - LTcprec_mm) %>% 
  ungroup() %>% 
  mutate(yearH = case_when(
    year == 2018 ~ "2018 (dry and hot)", 
    year == 2019 ~ "2019 (wet and hot)", 
    TRUE ~ "1990-2020"
  )) %>% 
  mutate(cprecip = round(max(LTcprec_mm))) 

w.p1 <-
  w2 %>% 
  mutate(facet = "Precipitation") %>% 
  ggplot() +
  geom_line(aes(doy, dprec_mm, group = year, color = yearH)) +
  geom_line(data = . %>% filter(year %in% c(2018, 2019)), 
            aes(doy, dprec_mm, group = year, color = yearH), linewidth = 1.2) +
  geom_hline(yintercept = 0) +
  geom_text(data = . %>% select(cprecip) %>% distinct(),
            aes(x = 20, y = 190, label = paste("Long-term mean precipitation = ", cprecip, " mm"), 
                hjust = 0, fontface = "italic")) +
  scale_color_manual(values = c("gray80", cv3, cv5)) +
  theme_bw() + 
 th1 +
  labs(x = "Day of year",
       y = "Deviation from long-term\nmean cumulative precipitation (mm)", 
       color = NULL) +
  facet_grid(.~facet)

p1

# temperature all year-------------------------------------------------------------

t3 <- 
  w %>% 
  group_by(year) %>% 
  mutate(dt_c = avgte - LTavgte,
         cdt_c = cumsum(dt_c)) %>% 
  ungroup() %>% 
  mutate(yearH = case_when(
    year == 2018 ~ "2018 (dry and hot)", 
    year == 2019 ~ "2019 (wet and hot)", 
    TRUE ~ "1990-2020"
  )) %>% 
  mutate(meanT = round(mean(LTavgte))) 


t3alt <- 
  w %>% 
  mutate(month = month(date2)) %>% 
  group_by(year, month) %>% 
  mutate(mmonth = mean(avgte)) %>% 
  group_by(month) %>% 
  mutate(mmonthLT = mean(avgte)) %>% 
  ungroup() %>% 
  mutate(yearH = case_when(
    year == 2018 ~ "2018 (dry and hot)", 
    year == 2019 ~ "2019 (wet and hot)", 
    TRUE ~ "1990-2020"
  )) 

w.p2 <- 
  t3 %>% 
  mutate(facet = "Air Temperature") %>% 
  ggplot() +
  geom_line(aes(doy, cdt_c, group = year, color = yearH)) +
  geom_line(data = . %>% filter(year %in% c(2018, 2019)), 
            aes(doy, cdt_c, group = year, color = yearH), linewidth = 1.2) +
  geom_hline(yintercept = 0) +
  geom_text(data = . %>% select(meanT) %>% distinct(),
            aes(x = 20, y = 480, label = paste("Long-term mean air temperature = ", meanT, " deg C"), 
                hjust = 0, fontface = "italic")) +
  scale_color_manual(values = c("gray80", cv3, cv5)) +
  theme_bw() + 
  th1 +
  labs(x = "Day of year",
       y = "Cumulative deviation from long-term\nmean air temperature (deg C - days)", 
       color = NULL) +
  facet_grid(.~facet)

p2


# yields ------------------------------------------------------------------

em_cc <- 
  read_csv("data/em_crop-yields-by-cc.csv") %>% 
  mutate(cctrt_nice = case_when(
    cctrt_id == "nocc" ~ "NoCC",
    cctrt_id == "mix_E" ~ "MixE",
    cctrt_id == "mix_M" ~ "MixM",
    cctrt_id == "rad_M" ~ "RadM",
    cctrt_id == "rad_L" ~ "RadL",
    TRUE~"XXX"
  ),
  cctrt_id = factor(cctrt_id, levels = ord.cctrt_id),
  cctrt_nice = factor(cctrt_nice, levels = ord.cctrt_nice))

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
  cctrt_nice = factor(cctrt_nice, levels = ord.cctrt_nice),
  crop = case_when(
    crop == "faba bean" ~ "Faba bean (2020)",
    crop == "oat" ~ "Oat (2019)",
    crop == "spring barley" ~ "Spring barley (2018)"
  ),
  crop = factor(crop, levels = ord.crop))

y.p1 <- 
  em_cc %>% 
  ggplot(aes(cctrt_nice, estimate)) +
  geom_jitter(data = d, aes(cctrt_nice, estimate, fill = crop, shape = crop), 
              width = 0.1, alpha = 0.3) +
  geom_errorbar(aes(x = cctrt_nice, ymin = estimate - std.error, ymax = estimate + std.error),
                width = 0.1, size = 1) +
  geom_point(size = 3) +
  geom_text(aes(x = cctrt_nice, y = estimate + 1.8, label = letter_sig)) +
  scale_y_continuous(limits = c(0, 6)) +
  scale_shape_manual(values = c(22, 21, 24)) +
  scale_fill_manual(values = c(cv3, av3, av4)) +
  theme_bw() +
  labs(x = NULL,
       y = myyieldlab,
       fill = NULL,
       shape = NULL) +
  theme(legend.position = c(0.3,0.15),
        legend.background = element_rect(fill = "transparent"))


# combine -----------------------------------------------------------------
w.p <- (w.p1 / w.p2) + plot_layout(guides = "collect") & theme(legend.position = "top")

w.p|y.p1

ggsave("figs/fig_weather.png", width = 5, height = 8)

# square plot -------------------------------------------------------------


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
    year == 2018 ~ "2018", 
    year == 2019 ~ "2019", 
    TRUE ~ "1990-2020"
  )) %>% 
  ggplot(aes(x = cprec_mm, y = avgT)) + 
  geom_point(aes(color = yearH), size = 8) +
  geom_vline(xintercept = LTprec) +
  geom_hline(yintercept = LTtemp) +
  geom_text(aes(x = 750, y = 7, label = "Cool and wet"), check_overlap = T, hjust = 0) +
  geom_text(aes(x = 750, y = 10, label = "Hot and wet"), check_overlap = T, hjust = 0) +
  geom_text(aes(x = 450, y = 7, label = "Cool and dry"), check_overlap = T, hjust = 0) +
  geom_text(aes(x = 450, y = 10, label = "Hot and dry"), check_overlap = T, hjust = 0) +
  scale_color_manual(values = c("gray80", "darkred", "blue"))

