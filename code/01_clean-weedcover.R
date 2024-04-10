# created 26/3/2024
# purpose: clean/separate Bo's CENTs data
# notes:


library(tidyverse)
library(readxl)

rm(list = ls())



# raw data ----------------------------------------------------------------

draw <- read_csv("data/raw/rd_gina-simplified.csv")



# percent weed coverage ---------------------------------------------

d2 <- 
  draw %>% 
  filter(is.na(yield_DM), !is.na(reg)) %>% 
  select(plot_id:date2, 
         rep = reg, 
         soil, volunteer, clover, lolpe, radish) %>%
  pivot_longer(soil:radish) 


# numbers don't add up to 100 ---------------------------------------------

#--cn use the largest remainder rule to make the percentages add to 100
#--tried to use chat gpt but it isn't working
#--some of the instances add up to 101. 

dtest <- 
  d2 %>%
  mutate(obs_id = paste(plot_id, date2, rep),
         obs_id = as.factor(obs_id),
         obs_id = as.numeric(obs_id))


dtest %>% 
  group_by(obs_id) %>% 
  mutate(tot = sum(value, na.rm = T)) %>% 
  ggplot(aes(obs_id, tot)) + 
  geom_point()

dtest %>% 
  group_by(obs_id) %>% 
  mutate(tot = sum(value, na.rm = T)) %>% 
filter(tot < 75)  

#--56 adds up to over 100. 
a.test <- 
  dtest %>% 
  filter(obs_id == 7) %>% 
  pull(value)


# write my own fucking function -------------------------------------------

atest1 <- c(21.311475, 13.114754,  9.836066, 54.098361,  0.000000)

add_values <- function(vector_length, num_times) {
  result <- numeric(vector_length)  # Initialize a vector of length vector_length with 0s
  for (i in 1:num_times) {
    result <- result + 1  # Add 1 to each element
    if (i %% vector_length == 0) {
      # If the index is a multiple of vector_length, reset it to 1 to recycle
      i <- i - vector_length
    }
  }
  return(result)
}

# Example usage:
vector_length <- 5
num_times <- 8
result <- add_values(vector_length, num_times)
print(result)


MakeThingsEqual100 <- function(values = atest1){
  
  d.tmp <- 
    tibble(org_values = values) %>% 
    mutate(values1 = replace_na(org_values, 0),
           values1_int = round(values1, 0),
           ord_id = paste0("n", 1:n())) %>% 
    arrange(-values1_int) %>% 
    mutate(tot1 = sum(values1_int))
  
  left_over <- 100  - d.tmp %>% pull(tot1) %>% unique()
  
  
  
  result <- numeric(nrow(d.tmp %>% 
                           filter(values1_int != 0)))  # Initialize a vector of length vector_length with 0s
  
  for (i in 1:left_over) {
    result <- result + 1  # Add 1 to each element
    if (i %% nrow(d.tmp %>% 
                  filter(values1_int != 0)) == 0) {
      # If the index is a multiple of vector_length, reset it to 1 to recycle
      i <- i - nrow(d.tmp %>% 
                      filter(values1_int != 0))
    }
  }
  
  result
  sort(values1_integers)
}


d3 <- 
  dtest %>% 
  group_by(plot_id, year, date, date2, rep) %>% 
  mutate(value2 = DistributeValues(value),
         tot = sum(value), 
         tot2 = sum(value2))

d3 %>% 
  ggplot(aes(tot, tot2)) + 
  geom_point()

d3 %>% 
  filter(tot2 > 100)

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

dnew <- 
  dtest %>% 
  filter(obs_id == 49) %>% 
  mutate(value_new = distribute_values(value)) %>% 
  mutate(tot = sum(value),
         tot2 = sum(value_new)) %>% 
  mutate(value_new2 = apply_largest_remainder_rule_grouped3(value_new))

tst <- c(33.1, 62.0, 0, 0, 0)
distribute_values(tst)

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





# chat gpt code -----------------------------------------------------------

distribute_values <- function(values, total_sum) {
  # Check for NA values and remove them
  non_na_values <- values[!is.na(values)]
  
  # Calculate the total number of non-NA values
  num_non_na <- sum(!is.na(values))
  
  # Check for zero values
  num_zeros <- sum(values == 0, na.rm = TRUE)
  
  # Calculate the integer part and remainder
  integer_part <- floor(non_na_values)
  remainder <- total_sum - sum(integer_part)
  
  # Handle the case where there are zero values
  if (num_zeros > 0) {
    # If there are zero values, distribute the remainder among them
    distributed_values <- integer_part
  } else {
    # Calculate the fractional part
    fractional_part <- non_na_values - integer_part
    
    # Calculate the shares based on fractional parts
    shares <- fractional_part / sum(fractional_part)
    
    # Calculate the number of shares to be allocated to each value
    allocated_shares <- floor(shares * remainder)
    
    # Calculate the remaining shares to be allocated
    remaining_shares <- remainder - sum(allocated_shares)
    
    # Sort the allocated shares and take the top 'remaining_shares' number of indices
    sorted_allocated_shares <- sort(allocated_shares, decreasing = TRUE)
    selected_indices <- order(-allocated_shares)[1:remaining_shares]
    
    # Add the remaining shares to the allocated values
    allocated_values <- integer_part
    allocated_values[selected_indices] <- allocated_values[selected_indices] + 1
    
    distributed_values <- allocated_values
  }
  
  return(distributed_values)
}

# Example usage:
values <- c(33.1, 62.0, 0, 0, 0)
total_sum <- 100

distributed_values <- distribute_values(values, total_sum)
distributed_values
sum(distributed_values)
