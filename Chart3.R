library("ggplot2")
library("dplyr")
library("scales")

#Ernest Hemingway's Book Checkouts Percentage by Material Type

np_data <- read.csv("~/Desktop/Ernest_Hemingway.csv", stringsAsFactors = FALSE)

book_data <- np_data %>%
  group_by(type = MaterialType) %>%
  summarise(checkouts = sum(Checkouts))

total_checkouts = sum(book_data$checkouts, na.rm = TRUE)

book_data <- book_data %>%
  mutate(percentage_checkouts = (checkouts / total_checkouts))

pie_chart <- ggplot(data = book_data, aes(x = "", y = percentage_checkouts, fill = type)) +
  labs(title = "Ernest Hemingway's Book Checkouts Percentage by Material Type from 2012 to 2023", fill = "Material Type") +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_brewer(palette = "Purples")
pie_chart