library(CENTSdata)
library(patchwork)
library(tidyverse)
rm(list = ls())


source("code/00_color-palettes.R")

w <- cents_wea

# precip all year ------------------------------------------------------------------

w2 <- 
  w %>% 
  group_by(year) %>% 
  mutate(cprec_mm = cumsum(prec_mm),
         dprec_mm = cprec_mm - LTcprec_mm) %>% 
  ungroup() %>% 
  mutate(yearH = case_when(
    year == 2018 ~ "2018 (Dry and hot)", 
    year == 2019 ~ "2019 (Wet and hot)", 
    TRUE ~ "1990-2020"
  )) %>% 
  mutate(cprecip = round(max(LTcprec_mm))) 

p1 <-
  w2 %>% 
  mutate(facet = "Precipitation") %>% 
  ggplot() +
  geom_line(aes(doy, dprec_mm, group = year, color = yearH)) +
  geom_hline(yintercept = 0) +
  geom_line(data = . %>% filter(year %in% c(2018, 2019)), 
            aes(doy, dprec_mm, group = year, color = yearH), linewidth = 1.2) +
  # geom_text(data = . %>% select(cprecip) %>% distinct(),
  #           aes(x = 20, y = 190, label = paste("Long-term mean precipitation = ", cprecip, " mm"), 
  #               hjust = 0, fontface = "italic")) +
  scale_color_manual(values = c("gray80", ylw1, dkbl1)) +
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
    year == 2018 ~ "2018 (Dry and hot)", 
    year == 2019 ~ "2019 (Wet and hot)", 
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
    year == 2018 ~ "2018 (hot and dry)", 
    year == 2019 ~ "2019 (hot and wet)", 
    TRUE ~ "1990-2020"
  )) 

p2 <- 
  t3 %>% 
  mutate(facet = "Air Temperature") %>% 
  ggplot() +
  geom_line(aes(doy, cdt_c, group = year, color = yearH)) +
  geom_hline(yintercept = 0) +
  geom_line(data = . %>% filter(year %in% c(2018, 2019)), 
            aes(doy, cdt_c, group = year, color = yearH), linewidth = 1.2) +
  # geom_text(data = . %>% select(meanT) %>% distinct(),
  #           aes(x = 20, y = 480, label = paste("Long-term mean air temperature = ", meanT, " deg C"), 
  #               hjust = 0, fontface = "italic")) +
  scale_color_manual(values = c("gray80", ylw1, dkbl1)) +
  theme_bw() + 
  th1 +
  labs(x = "Day of year",
       y = "Cumulative deviation from long-term\nmean air temperature (deg C - days)", 
       color = NULL) +
  facet_grid(.~facet)

p2

#p2alt <- 
  # t3alt %>% 
  # mutate(facet = "Air Temperature") %>% 
  # ggplot() +
  # geom_line(aes(month, mmonth, group = year, color = yearH)) +
  # geom_line(data = . %>% filter(year %in% c(2018, 2019)), 
  #           aes(month, mmonth, group = year, color = yearH), linewidth = 1.2) +
  # geom_hline(yintercept = 0) +
  # geom_text(data = . %>% select(mmonthLT) %>% distinct(),
  #           aes(x = 20, y = 480, label = paste("Long-term mean air temperature = ", meanT, " deg C"), 
  #               hjust = 0, fontface = "italic")) +
  # scale_color_manual(values = c("gray80", cv3, cv5)) +
  # theme_bw() + 
  # th1 +
  # labs(x = "Day of year",
  #      y = "Deviation from long-term\nmean air temperature (deg C)", 
  #      color = NULL) +
  # facet_grid(.~facet)



# combine -----------------------------------------------------------------
# p1 / p2 + plot_layout(guides = "collect") & theme(legend.position = "top")
# 
# ggsave("figs/fig_weather.png", width = 5, height = 8)

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
    year == 2018 ~ "2018", 
    year == 2019 ~ "2019", 
    TRUE ~ "1990-2020"
  )) %>% 
  mutate(fakefacet = "Summary")

p3 <- 
  ggplot() + 
  geom_vline(xintercept = LTprec) +
  geom_hline(yintercept = LTtemp) +
  geom_point(data = d3 %>% filter(yearH == "1990-2020"),
             aes(x = cprec_mm, y = avgT), 
             size = 8, color = "gray80") +
  geom_point(data = d3 %>% filter(yearH == "2018"),
             aes(x = cprec_mm, y = avgT), 
             size = 8, color = ylw1) +
  geom_point(data = d3 %>% filter(yearH == "2019"),
             aes(x = cprec_mm, y = avgT), 
             size = 8, color = dkbl1) +
  geom_text(aes(x = 675, y = 7, label = "Wet and cool"), 
            check_overlap = T, hjust = 0, fontface = "italic") +
  geom_text(aes(x = 675, y = 10, label = "Wet and hot"), 
            check_overlap = T, hjust = 0, fontface = "italic") +
  geom_text(aes(x = 450, y = 7, label = "Dry and cool"), 
            check_overlap = T, hjust = 0, fontface = "italic") +
  geom_text(aes(x = 450, y = 10, label = "Dry and hot"), 
            check_overlap = T, hjust = 0, fontface = "italic") +
  scale_color_manual(values = c("gray80", ylw1, dkbl1)) +
  theme_bw() +
  th1 +
  labs(x = "Mean cumulative precipitation\n(mm)",
       y = "Mean air temperatue\n(deg C)",
       color = NULL)+
  facet_grid(.~fakefacet)

p3



# trying to piece together ------------------------------------------------

#--other formats

design <- "
3344
1122
"    
p1 + p2 + p3 + guide_area() + plot_layout(design=design, guides = "collect") 


ggsave("figs/fig_weather.png", width = 8, height = 8)
