library("ggplot2")
library("dplyr")
library("scales")

#The Short Stories checkouts by year since 2012

np_data <- read.csv("~/Desktop/The_Short_Stories.csv", stringsAsFactors = FALSE)

book_data <- np_data %>%
  select(year = CheckoutYear, title = Title,checkouts = Checkouts)

book_data <- book_data %>%
  mutate(summarise_title = gsub("\\(.*","", title))

book_data <- book_data %>%
  mutate(summarise_title2 = gsub("III ","III", summarise_title))

book_data <- book_data %>%
  mutate(summarise_title3 = gsub("II ","II", summarise_title2))

book_data <- book_data %>%
  mutate(summarise_title4 = gsub("of Ernest Hemingway:"," ", summarise_title3))

book_data <- book_data %>%
  mutate(Title = gsub("   ","", summarise_title4))

book_data <- book_data %>%
  group_by(year, Title) %>%
  summarise(checkouts = sum(checkouts))

line_chart <- ggplot(data = book_data, aes(x = year, y = checkouts, color = Title)) +
  labs(title = "The Short Stories Checkouts by Year since 2012", x = "Year", y = "Amount of Checkouts") +
  geom_line() +
  geom_point()
