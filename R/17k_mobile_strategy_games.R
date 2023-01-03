library(tidyverse)
library(ggrepel)
rm(list = ls())

# Load dataset into a tibble.
mobile <- read_csv("datasets/17k_mobile_strategy_games.csv")

# Get to know the tibble
## Print the number of rows, observations; number of columns, variables; and 
## the variable names.
nrow(mobile)
ncol(mobile)
names(mobile)

# Inspect the tibble
## Print the head of tibble to see the different variables and their types.
head(mobile)

## Print a summary of the tibble to get an idea of the range of the variables.
summary(mobile)

## To make it more concise we may want to only inspect the numerical variables.
mobile %>%
  select_if(is.numeric) %>%
  summary()

mobile <- mobile %>%
  rename(AvgUserRating = `Average User Rating`,
         InAppPurchases = `In-app Purchases`)

# Handle missing values
# TODO: Add something about imputation even though we do not use it in the 
# course.
## Summarise NAs across all columns in the tibble 
mobile %>% 
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  summary()
## Maybe it is more interesting to show the percentage of NAs
mobile %>% 
  summarise(across(everything(), ~ sum(is.na(.)) / n())) %>%
  summary()
# Select only the columns with NAs
mobile %>% 
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  select(which(colMeans(.) > 0)) %>%
  summary()
# Look at which columns have NA values
mobile %>% 
  select(c(AvgUserRating, Price)) %>%
  ggplot(aes(x = AvgUserRating, y = Price)) + 
  geom_point()