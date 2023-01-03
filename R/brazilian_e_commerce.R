library(tidyverse)
library(ggrepel)
rm(list = ls())

# Load dataset into a tibble.
customers <- read_csv("datasets/brazilian_e-commerce/olist_customers_dataset.csv")
summary(customers)

orders <- read_csv("datasets/brazilian_e-commerce/olist_orders_dataset.csv")
summary(orders)

items <- read_csv("datasets/brazilian_e-commerce/olist_order_items_dataset.csv")
summary(items)

reviews <- read_csv("datasets/brazilian_e-commerce/olist_order_reviews_dataset.csv")
summary(reviews)

df <- inner_join(reviews, items, by = "order_id")
summary(df)

# Check for NAs
df %>%
  select(c(price, review_score)) %>%
  summarise(across(everything(), ~ sum(is.na(.))))

df %>%
  ggplot(aes(x = review_score, y = price)) +
  geom_point()

df %>%
  filter(review_score > 1) %>%
  ggplot(aes(x = price, y = review_score)) +
  geom_point()

df_lm <- df %>%
  filter(review_score > 1)

lm(price ~ review_score, df_lm)

df %>%
  filter(review_score == 3) %>%
  ggplot(aes(price)) +
  geom_histogram()
