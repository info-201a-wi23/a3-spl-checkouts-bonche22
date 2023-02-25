library("ggplot2")
library("dplyr")
library("scales")

#Ratio of Ernest Hemingway's book checkouts subjects by year

np_data <- read.csv("~/Desktop/Ernest_Hemingway.csv", stringsAsFactors = FALSE)

book_data <- np_data %>%
  select(year = CheckoutYear, Subjects = Subjects, Checkouts = Checkouts)

book_data <- book_data %>%
  mutate(summarise_subjects = gsub("Classic Literature.*","Classic Literature", Subjects))

book_data <- book_data %>%
  mutate(summarise_subjects2 = gsub("Biography & Autobiography,.*","Biography & Autobiography", summarise_subjects))

book_data <- book_data %>%
  mutate(summarise_subjects3 = gsub("Fiction,.*","Fiction", summarise_subjects2))

book_data <- book_data %>%
  mutate(summarise_subjects4 = gsub("History,.*","History", summarise_subjects3))

book_data <- book_data %>%
  mutate(summarise_subjects5 = gsub("Literary Criticism,.*","Literary Criticism", summarise_subjects4))

book_data <- book_data %>%
  mutate(Subjects = gsub("Nonfiction,.*","Nonfiction", summarise_subjects5))

book_data <- book_data %>%
  group_by(Subjects, year) %>%
  summarise(checkouts = sum(Checkouts))

book_data <- book_data[-1,]

bar_chart <- ggplot(book_data, aes(x = year, y = checkouts)) +
  geom_bar(stat = "identity", aes(fill = Subjects)) +
  labs(title = "Ernest Hemingway's Book Checkouts Subjects by Year since 2012", x = "Year", y = "Amount of Checkouts") +
  theme(plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7)) +
  scale_fill_brewer(palette = "RdPu")
