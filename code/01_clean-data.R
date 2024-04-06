# created 26/3/2024
# purpose: clean/separate Bo's CENTs data
# notes:


library(tidyverse)
library(readxl)

rm(list = ls())



# raw data ----------------------------------------------------------------

draw <- read_csv("data/raw/rd_gina-simplified.csv")



# 1. yields ---------------------------------------------------------------

d1 <-
  draw %>% 
  filter(!is.na(yield_DM)) %>% 
  select(plot_id:date2, 
         yield_dry_gm2 = yield_DM, 
         yield_15p_tonha = yield15) %>% 
  mutate(crop = case_when(
    year == 2018 ~ "barley",
    year == 2019 ~ "oat",
    year == 2020 ~ "Not sure"))

d1 %>% 
  ggplot(aes(year, yield_15p_tonha)) + 
  geom_jitter(aes(color = crop), width = 0.2, size = 2)

d1 %>% 
  select(plot_id, year, crop, date, date2, everything()) %>% 
  write_csv("data/tidy/td_yield.csv")


# need to do percent coverage ---------------------------------------------

d2 <- 
  draw %>% 
  filter(is.na(yield_DM), !is.na(reg)) %>% 
  select(plot_id:date2, 
         rep = reg, 
         soil, volunteer, clover, lolpe, radish) %>%
  pivot_longer(soil:radish) 


#--cn use the largest remainder rule to make the percentages add to 100
#--tried to use chat gpt but it isn't working

d3 <- 
  d2 %>% 
  group_by(plot_id, year, date, date2, rep) %>% 
  mutate(value2 = apply_largest_remainder_rule_grouped5(value),
         value3 = apply_largest_remainder_rule_grouped5(value2),
         tot = sum(value), 
         tot2 = sum(value3))

d3 %>% 
  ggplot(aes(tot, tot2)) + 
  geom_point()

#---argh what is wrong? 

#--not sure what group it isn't working in
dtest <- 
  d2 %>%
  mutate(obs_id = paste(plot_id, date2, rep),
         obs_id = as.factor(obs_id),
         obs_id = as.numeric(obs_id))


#--group 49 breaks it
a.test <- 
  dtest %>% 
  filter(obs_id == 49) %>% 
  pull(value)

#--ok so you have to run it through twice?
dnew <- 
  dtest %>% 
  filter(obs_id == 49) %>% 
  mutate(value_new = apply_largest_remainder_rule_grouped3(value)) %>% 
  mutate(tot = sum(value),
         tot2 = sum(value_new)) %>% 
  mutate(value_new2 = apply_largest_remainder_rule_grouped3(value_new))


for (i in 2:1440){
  
  d.tmp <- 
    dtest %>% 
    filter(obs_id == i) %>% 
    mutate(value_new = apply_largest_remainder_rule_grouped(value)) %>% 
    mutate(tot = sum(value),
           tot2 = sum(value_new))
  
  dnew <- 
    dnew %>% 
    bind_rows(d.tmp)
  
  print(i)
  
}





#--chat gpt code
# Function to apply largest remainder rule within groups, handling NA values
apply_largest_remainder_rule_grouped <- function(values, total_percentage = 100) {
  #values <- a.test
  # Check for NA values
  if (any(is.na(values))) {
    return(rep(NA, length(values)))
  }
  
  # Calculate the floor of each percentage
  floor_values <- floor(values)
  # Calculate the remainders
  remainders <- values - floor_values
  # Calculate the total percentage allocated to the floor values
  total_floor_percentage <- sum(floor_values)
  # Calculate the remaining percentage to be distributed
  remaining_percentage <- total_percentage - total_floor_percentage
  # Find the indices of the largest remainders
  largest_remainders <- order(remainders, decreasing = TRUE)[1:remaining_percentage]
  # Add 1 to the floor values with the largest remainders
  floor_values[largest_remainders] <- floor_values[largest_remainders] + 1
  return(floor_values)
}

# Example dataframe with NA values
df <- tibble(
  group = c("Group1", "Group1", "Group2", "Group2", "Group3"),
  category = c("A", "B", "C", "D", "E"),
  percentage = c(23.4, 45.6, NA, 18.7, 29)
)

# Apply largest remainder rule within each group
df <- df %>%
  group_by(group) %>%
  mutate(percentage = apply_largest_remainder_rule_grouped(percentage))

print(df)

# Function to apply largest remainder rule within groups, handling NA values and ensuring sum equals total percentage
apply_largest_remainder_rule_grouped2 <- function(values, total_percentage = 100) {
  # Check for NA values
  if (any(is.na(values))) {
    return(rep(NA, length(values)))
  }
  
  # Count non-zero values
  non_zero_count <- sum(values != 0)
  
  # Check if there are fewer non-zero values than total percentage
  if (non_zero_count < total_percentage) {
    return(rep(NA, length(values)))
  }
  
  # Calculate the floor of each percentage
  floor_values <- floor(values)
  # Calculate the remainders
  remainders <- values - floor_values
  # Calculate the total percentage allocated to the floor values
  total_floor_percentage <- sum(floor_values)
  # Calculate the remaining percentage to be distributed
  remaining_percentage <- total_percentage - total_floor_percentage
  # Find the indices of the largest remainders
  largest_remainders <- order(remainders, decreasing = TRUE)[1:remaining_percentage]
  # Add 1 to the floor values with the largest remainders
  floor_values[largest_remainders] <- floor_values[largest_remainders] + 1
  return(floor_values)
}

# Example dataframe with two values and three zeros
df <- tibble(
  group = c("Group1", "Group1", "Group2", "Group2", "Group3"),
  category = c("A", "B", "C", "D", "E"),
  percentage = c(0, 0, 50, 50, 0)
)

# Apply largest remainder rule within each group
df <- df %>%
  group_by(group) %>%
  mutate(percentage = apply_largest_remainder_rule_grouped(percentage))

print(df)

# Function to apply largest remainder rule within groups, handling NA values and ensuring sum equals total percentage
apply_largest_remainder_rule_grouped3 <- function(values, total_percentage = 100) {
  # Check for NA values
  if (any(is.na(values))) {
    return(rep(NA, length(values)))
  }
  
  # Count non-zero values
  non_zero_values <- values[values != 0]
  non_zero_count <- length(non_zero_values)
  
  # Check if there are fewer non-zero values than total percentage
  if (non_zero_count == 0) {
    return(rep(0, length(values)))
  }
  
  # Calculate the floor of each percentage
  floor_values <- floor(values)
  # Calculate the remainders
  remainders <- values - floor_values
  # Calculate the total percentage allocated to the floor values
  total_floor_percentage <- sum(floor_values)
  # Calculate the remaining percentage to be distributed
  remaining_percentage <- total_percentage - total_floor_percentage
  
  # If there are fewer non-zero values than total percentage,
  # distribute the remaining percentage equally among non-zero values
  if (non_zero_count < total_percentage) {
    extra_percentage <- remaining_percentage %% non_zero_count
    floor_values[values != 0] <- floor_values[values != 0] + (remaining_percentage %/% non_zero_count)
    floor_values[order(remainders[values != 0], decreasing = TRUE)[1:extra_percentage]] <- floor_values[order(remainders[values != 0], decreasing = TRUE)[1:extra_percentage]] + 1
  } else {
    # Find the indices of the largest remainders
    largest_remainders <- order(remainders, decreasing = TRUE)[1:remaining_percentage]
    # Add 1 to the floor values with the largest remainders
    floor_values[largest_remainders] <- floor_values[largest_remainders] + 1
  }
  
  return(floor_values)
}


# Function to apply largest remainder rule within groups, handling NA values and ensuring sum equals total percentage
apply_largest_remainder_rule_grouped4 <- function(values, total_percentage = 100) {
  # Check for NA values
  if (any(is.na(values))) {
    return(rep(NA, length(values)))
  }
  
  # Calculate the sum of percentages
  sum_values <- sum(values, na.rm = TRUE)
  
  # If sum of percentages exceeds total percentage, distribute proportionally
  if (sum_values > total_percentage) {
    excess_percentage <- sum_values - total_percentage
    values <- values * (total_percentage / sum_values)
  }
  
  # Count non-zero values
  non_zero_values <- values[values != 0]
  non_zero_count <- length(non_zero_values)
  
  # Check if there are fewer non-zero values than total percentage
  if (non_zero_count == 0) {
    return(rep(0, length(values)))
  }
  
  # Calculate the floor of each percentage
  floor_values <- floor(values)
  # Calculate the remainders
  remainders <- values - floor_values
  # Calculate the total percentage allocated to the floor values
  total_floor_percentage <- sum(floor_values)
  # Calculate the remaining percentage to be distributed
  remaining_percentage <- total_percentage - total_floor_percentage
  
  # If there are fewer non-zero values than total percentage,
  # distribute the remaining percentage equally among non-zero values
  if (non_zero_count < total_percentage) {
    extra_percentage <- remaining_percentage %% non_zero_count
    floor_values[values != 0] <- floor_values[values != 0] + (remaining_percentage %/% non_zero_count)
    floor_values[order(remainders[values != 0], decreasing = TRUE)[1:extra_percentage]] <- floor_values[order(remainders[values != 0], decreasing = TRUE)[1:extra_percentage]] + 1
  } else {
    # Find the indices of the largest remainders
    largest_remainders <- order(remainders, decreasing = TRUE)[1:remaining_percentage]
    # Add 1 to the floor values with the largest remainders
    floor_values[largest_remainders] <- floor_values[largest_remainders] + 1
  }
  
  return(floor_values)
}

# Example dataframe with two values and three zeros
df <- tibble(
  group = c("Group1", "Group1", "Group2", "Group2", "Group3"),
  category = c("A", "B", "C", "D", "E"),
  percentage = c(0, 0, 50, 50, 0)
)

# Apply largest remainder rule within each group
df <- df %>%
  group_by(group) %>%
  mutate(percentage = apply_largest_remainder_rule_grouped(percentage))

print(df)


# Function to apply largest remainder rule within groups, handling NA values and ensuring sum equals total percentage
apply_largest_remainder_rule_grouped5 <- function(values, total_percentage = 100) {
  # Check for NA values
  if (any(is.na(values))) {
    return(rep(NA, length(values)))
  }
  
  # Calculate the sum of percentages
  sum_values <- sum(values, na.rm = TRUE)
  
  # If sum of percentages exceeds total percentage, truncate the excess
  if (sum_values > total_percentage) {
    excess_percentage <- sum_values - total_percentage
    values <- values - (values / sum_values) * excess_percentage
  }
  
  # Count non-zero values
  non_zero_values <- values[values != 0]
  non_zero_count <- length(non_zero_values)
  
  # Check if there are fewer non-zero values than total percentage
  if (non_zero_count == 0) {
    return(rep(0, length(values)))
  }
  
  # Calculate the floor of each percentage
  floor_values <- floor(values)
  # Calculate the remainders
  remainders <- values - floor_values
  # Calculate the total percentage allocated to the floor values
  total_floor_percentage <- sum(floor_values)
  # Calculate the remaining percentage to be distributed
  remaining_percentage <- total_percentage - total_floor_percentage
  
  # If there are fewer non-zero values than total percentage,
  # distribute the remaining percentage equally among non-zero values
  if (non_zero_count < total_percentage) {
    extra_percentage <- remaining_percentage %% non_zero_count
    floor_values[values != 0] <- floor_values[values != 0] + (remaining_percentage %/% non_zero_count)
    floor_values[order(remainders[values != 0], decreasing = TRUE)[1:extra_percentage]] <- floor_values[order(remainders[values != 0], decreasing = TRUE)[1:extra_percentage]] + 1
  } else {
    # Find the indices of the largest remainders
    largest_remainders <- order(remainders, decreasing = TRUE)[1:remaining_percentage]
    # Add 1 to the floor values with the largest remainders
    floor_values[largest_remainders] <- floor_values[largest_remainders] + 1
  }
  
  return(floor_values)
}

