install.packages("tidyverse")
library("ggplot2")
library("dplyr")
library("scales")

#The Short Stories checkouts by year since 2012

np_data <- read.csv("~/Desktop/The_Short_Stories.csv", stringsAsFactors = FALSE)

book_data <- np_data %>%
  group_by(year = CheckoutYear, title = Title) %>%
  summarise(checkouts = sum(Checkouts))

book_data <- book_data %>%
  mutate(summarise_title = gsub("  ","", title))

book_data <- book_data %>%
  mutate(summarise_title2 = gsub("\\(.*","", summarise_title))

book_data <- book_data %>%
  mutate(Title = gsub("III .*","", summarise_title2))


line_chart <- ggplot(data = book_data, aes(x = year, y = checkouts, color = Title)) +
  labs(title = "The Short Stories Checkouts by Year since 2012", x = "Year", y = "Amount of Checkouts") +
  geom_line() +
  geom_point()
