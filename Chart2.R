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
  mutate(summarise_title4 = gsub("\\:.*","", summarise_title3))

book_data <- book_data %>%
  mutate(summarise_title5 = gsub("\\ I.*","", summarise_title4))

book_data <- book_data %>%
  mutate(summarise_title6 = gsub("\\of Ernest Hemingway.*","", summarise_title5))

book_data <- book_data %>%
  mutate(Title = gsub("\\ Volume.*","", summarise_title6))

book_data <- book_data %>%
  group_by(Title, year) %>%
  summarise(total_checkouts = sum(checkouts))

bar_chart <- ggplot(book_data, aes(x = year, y = total_checkouts)) +
  geom_bar(stat = "identity", aes(fill = Title)) +
  labs(title = "Ernest Hemingway's Book Checkouts by Year", x = "Year", y = "Amount of Checkouts") +
  theme(legend.text = element_text(size = 5))   
