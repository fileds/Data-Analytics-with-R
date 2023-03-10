library(tidyverse)
library(ggrepel)
rm(list = ls())

# Load dataset into a tibble.
titanic <- read_csv("datasets/titanic.csv")

# Get to know the tibble
## Print the number of rows, observations; number of columns, variables; and 
## the variable names.
nrow(titanic)
ncol(titanic)
names(titanic)

# Format the columns so they are understandable
## Change the name of a column
titanic <- titanic %>%
  rename(TicketClass = Pclass)

## Print the result
names(titanic)

# Inspect the tibble
## Print the head of tibble to see the different variables and their types.
head(titanic)

## Print a summary of the tibble to get an idea of the range of the variables.
summary(titanic)

## To make it more concise we may want to only inspect the numerical variables.
titanic %>%
  select_if(is.numeric) %>%
  summary()

# Transform the tibble
## Convert the 'Sex' variable to a factor.
titanic <- titanic %>%
  mutate(Sex = factor(Sex))
head(titanic)

# Handle missing values
# TODO: Add something about imputation even though we do not use it in the 
# course.
## Summarise NAs across all columns in the tibble 
titanic %>% 
  summarise(across(everything(), ~ sum(is.na(.))))

## Maybe it is more interesting to show the percentage of NAs
titanic %>% 
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  mutate(across(everything(), ~ . / nrow(titanic)))

## Drop the cabin column as it has a lot of NAs
titanic <- titanic %>%
  select(!Cabin)
head(titanic)
  
## Drop all rows containing NAs
titanic <- titanic %>% 
  na.omit()

titanic %>%
  ggplot(aes(x = Age, y = Fare, label = Age)) + 
  geom_point() +
  geom_text_repel()

# Logistic regression for survival prediction
# TODO: Use training and test data
## Fit a logistic regression model
model <- glm("Survived ~ Age + Fare", data = titanic, family = "binomial")

model$coefficients

# Chi squared test of Sex and Survived. Spoiler alert: No men survived
chisq.test(titanic$Sex, titanic$Survived)

titanic %>%
  ggplot(aes(x = Sex, y = Survived)) +
  geom_boxplot()

titanic %>%
  group_by(Sex) %>%
  summarize(Count = n(),
            Survived = sum(Survived),
            Survived_prcnt = sum(Survived) / n())

# t test of different price of men and women
titanic %>%
  ggplot(aes(x = Fare, col = Sex)) +
  geom_density()
titanic %>%
  ggplot(aes(x = TicketClass, fill = Sex)) +
  geom_bar() +
  theme_bw()
titanic %>%
  ggplot(aes(x = TicketClass, fill = Sex)) +
  geom_bar(position = "fill") +
  theme_bw()

mens_fare <- titanic %>% 
  filter(Sex == "male") %>%
  select(Fare)
womens_fare <- titanic %>% 
  filter(Sex == "female") %>%
  select(Fare)

fares <- titanic %>%
  select(Sex, Fare)
  
t.test(select(filter(fares, Sex == "male"), Fare),
       select(filter(fares, Sex == "female"), Fare))
