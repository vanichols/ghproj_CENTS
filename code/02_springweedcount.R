# created 15 oct 2024
# purpose: do stats on crop yields, using new CENTSdata package
# notes: starting with basic response variable w/maarit's help

library(tidyverse)
library(lme4)
library(lmerTest)
library(CENTSdata)

rm(list = ls())

# data --------------------------------------------------------------------

eu <- as_tibble(cents_eukey)
sp <- as_tibble(cents_spweedcount)

d <- eu %>% left_join(y)

