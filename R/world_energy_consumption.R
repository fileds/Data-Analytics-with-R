library(tidyverse)
library(ggrepel)
rm(list = ls())

# Load dataset into a tibble.
energy <- read_csv("../datasets/world_energy_consumption.csv")

# Get to know the tibble
## Print the number of rows, observations; number of columns, variables; and 
## the variable names.
nrow(energy)
ncol(energy)
names(energy)

# Inspect the tibble
## Print the head of tibble to see the different variables and their types.
head(energy)

## Print a summary of the tibble to get an idea of the range of the variables.
summary(energy)

## Inspect the variable 'year' more closely
summary(energy$year)

## Identify the last year in the data
latest <- max(energy$year)

## To make it more concise we may want to only inspect the numerical variables.
energy %>%
  select_if(is.numeric) %>%
  summary()

# Handle missing values
## Summarise NAs across all columns in the tibble 
energy %>% 
  filter(year == latest) %>%
  summarise(across(everything(), ~ sum(is.na(.))))

## Maybe it is more interesting to show the percentage of NAs
energy %>% 
  filter(year == latest) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  mutate(across(everything(), ~ . / nrow(energy)))

## Inspect the countries with missing values
energy %>%
  filter(year == latest - 1) %>%
  select(country, gdp, solar_elec_per_capita)
         
gdp <- energy %>%
  select(country, year, gdp, solar_elec_per_capita) %>%
  filter(year == 2010)

gdp %>% 
  ggplot(aes(x = gdp, y = solar_elec_per_capita, label = country)) +
  geom_point() + 
  geom_text_repel(
    xlim = c(2.5e13, NA),
    direction = "y"
    )

energy %>% 
  filter(year == 2010) %>%
  filter(!(country == "World")) %>%
  ggplot(aes(x = gdp, y = solar_elec_per_capita, col = nuclear_electricity, label = country)) +
  geom_point() + 
  geom_text_repel(
    xlim = c(5e12, NA),
    direction = "y"
    )
  
