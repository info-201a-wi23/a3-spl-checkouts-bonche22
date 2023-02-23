install.packages("tidyverse")
library("ggplot2")
library("dplyr")
library("scales")

#Ratio of Ernest Hemingway's book checkouts by year

np_data <- read.csv("~/Desktop/Ernest_Hemingway.csv", stringsAsFactors = FALSE)

book_data <- np_data %>%
  select(year = CheckoutYear, title = Title, checkouts = Checkouts)

book_data <- book_data %>%
  mutate(summarise_title = gsub("  ","", title))

book_data <- book_data %>%
  mutate(summarise_title2 = gsub("\\(.*","", summarise_title))

book_data <- book_data %>%
  mutate(summarise_title3 = gsub("III .*","", summarise_title2))

book_data <- book_data %>%
  mutate(Title = gsub("\\:.*","", summarise_title3))

book_data <- book_data %>%
  group_by(Title, year) %>%
  summarise(total_checkouts = sum(checkouts))

ggplot(book_data, aes(x = year, y = total_checkouts)) +
  geom_bar(stat = "identity", aes(fill = Title)) +
  labs(title = "Ernest Hemingway's Book Checkouts by Year", x = "Year", y = "Amount of Checkouts")