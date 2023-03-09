library("dplyr")

#data
np_the_short_story_data <- read.csv("~/Desktop/The_Short_Stories.csv", stringsAsFactors = FALSE)
np_Hemingway_data <- read.csv("~/Desktop/Ernest_Hemingway.csv", stringsAsFactors = FALSE)


#The total checkouts of each book from The Short Story Series by year
TST_total_year_data <- np_the_short_story_data %>%
  select(year = CheckoutYear, title = Title, checkouts = Checkouts) %>%
  mutate(summarise_title = gsub("\\(.*","", title)) %>%
  mutate(summarise_title2 = gsub("III ","III", summarise_title)) %>%
  mutate(summarise_title3 = gsub("II ","II", summarise_title2)) %>%
  mutate(summarise_title4 = gsub("of Ernest Hemingway:"," ", summarise_title3)) %>%
  mutate(Title = gsub("   ","", summarise_title4)) %>%
  group_by(year, Title) %>%
  summarise(checkouts = sum(checkouts))


#The total checkouts of The Short Stories Volume I
TST_I_total_checkouts <- np_the_short_story_data %>%
  select(year = CheckoutYear, title = Title, checkouts = Checkouts) %>%
  mutate(summarise_title = gsub("\\(.*","", title)) %>%
  mutate(summarise_title2 = gsub("III ","III", summarise_title)) %>%
  mutate(summarise_title3 = gsub("II ","II", summarise_title2)) %>%
  mutate(summarise_title4 = gsub("of Ernest Hemingway:"," ", summarise_title3)) %>%
  mutate(Title = gsub("   ","", summarise_title4)) %>%
  group_by(Title) %>%
  filter(Title %in% c("The Short Stories Volume I ")) %>%
  summarise(checkouts = sum(checkouts)) %>%
  pull(checkouts)


#The total checkouts of The Short Stories Volume II
TST_II_total_checkouts <- np_the_short_story_data %>%
  select(year = CheckoutYear, title = Title, checkouts = Checkouts) %>%
  mutate(summarise_title = gsub("\\(.*","", title)) %>%
  mutate(summarise_title2 = gsub("III ","III", summarise_title)) %>%
  mutate(summarise_title3 = gsub("II ","II", summarise_title2)) %>%
  mutate(summarise_title4 = gsub("of Ernest Hemingway:"," ", summarise_title3)) %>%
  mutate(Title = gsub("   ","", summarise_title4)) %>%
  group_by(Title) %>%
  filter(Title %in% c("The Short Stories Volume II")) %>%
  summarise(checkouts = sum(checkouts)) %>%
  pull(checkouts)


#The total checkouts of The Short Stories Volume III
TST_III_total_checkouts <- np_the_short_story_data %>%
  select(year = CheckoutYear, title = Title, checkouts = Checkouts) %>%
  mutate(summarise_title = gsub("\\(.*","", title)) %>%
  mutate(summarise_title2 = gsub("III ","III", summarise_title)) %>%
  mutate(summarise_title3 = gsub("II ","II", summarise_title2)) %>%
  mutate(summarise_title4 = gsub("of Ernest Hemingway:"," ", summarise_title3)) %>%
  mutate(Title = gsub("   ","", summarise_title4)) %>%
  group_by(Title) %>%
  filter(Title %in% c("The Short Stories Volume III")) %>%
  summarise(checkouts = sum(checkouts)) %>%
  pull(checkouts)


#The total checkouts of all The Short Stories
TST_III_total_checkouts <- np_the_short_story_data %>%
  select(year = CheckoutYear, title = Title, checkouts = Checkouts) %>%
  mutate(summarise_title = gsub("\\(.*","", title)) %>%
  mutate(summarise_title2 = gsub("III ","III", summarise_title)) %>%
  mutate(summarise_title3 = gsub("II ","II", summarise_title2)) %>%
  mutate(summarise_title4 = gsub("of Ernest Hemingway:"," ", summarise_title3)) %>%
  mutate(Title = gsub("   ","", summarise_title4)) %>%
  summarise(checkouts = sum(checkouts)) %>%
  pull(checkouts)


#The total checkouts of Hemingway's book of each subjects by year
Hemingway_subject_data <- np_Hemingway_data %>%
  select(year = CheckoutYear, Subjects = Subjects, Checkouts = Checkouts) %>%
  mutate(summarise_subjects = gsub("Classic Literature.*","Classic Literature", Subjects)) %>%
  mutate(summarise_subjects2 = gsub("Biography & Autobiography,.*","Biography & Autobiography", summarise_subjects)) %>%
  mutate(summarise_subjects3 = gsub("Fiction,.*","Fiction", summarise_subjects2)) %>%
  mutate(summarise_subjects4 = gsub("History,.*","History", summarise_subjects3)) %>%
  mutate(summarise_subjects5 = gsub("Literary Criticism,.*","Literary Criticism", summarise_subjects4)) %>%
  mutate(Subjects = gsub("Nonfiction,.*","Nonfiction", summarise_subjects5)) %>%
  group_by(Subjects, year) %>%
  summarise(checkouts = sum(Checkouts))

total_Hemingway_data <- total_Hemingway_data[-1,]


#The total checkouts of Hemingway's book of the subject Biography & Autobiography
Hemingway_Biography_Autobiography <- np_Hemingway_data %>%
  select(year = CheckoutYear, Subjects = Subjects, Checkouts = Checkouts) %>%
  mutate(summarise_subjects = gsub("Classic Literature.*","Classic Literature", Subjects)) %>%
  mutate(summarise_subjects2 = gsub("Biography & Autobiography,.*","Biography & Autobiography", summarise_subjects)) %>%
  mutate(summarise_subjects3 = gsub("Fiction,.*","Fiction", summarise_subjects2)) %>%
  mutate(summarise_subjects4 = gsub("History,.*","History", summarise_subjects3)) %>%
  mutate(summarise_subjects5 = gsub("Literary Criticism,.*","Literary Criticism", summarise_subjects4)) %>%
  mutate(Subjects = gsub("Nonfiction,.*","Nonfiction", summarise_subjects5)) %>%
  group_by(Subjects) %>%
  filter(Subjects %in% c("Biography & Autobiography")) %>%
  summarise(checkouts = sum(Checkouts)) %>%
  pull(checkouts)


#The total checkouts of Hemingway's book of the subject Classic Literature
Hemingway_Classic_Literature <- np_Hemingway_data %>%
  select(year = CheckoutYear, Subjects = Subjects, Checkouts = Checkouts) %>%
  mutate(summarise_subjects = gsub("Classic Literature.*","Classic Literature", Subjects)) %>%
  mutate(summarise_subjects2 = gsub("Biography & Autobiography,.*","Biography & Autobiography", summarise_subjects)) %>%
  mutate(summarise_subjects3 = gsub("Fiction,.*","Fiction", summarise_subjects2)) %>%
  mutate(summarise_subjects4 = gsub("History,.*","History", summarise_subjects3)) %>%
  mutate(summarise_subjects5 = gsub("Literary Criticism,.*","Literary Criticism", summarise_subjects4)) %>%
  mutate(Subjects = gsub("Nonfiction,.*","Nonfiction", summarise_subjects5)) %>%
  group_by(Subjects) %>%
  filter(Subjects %in% c("Classic Literature")) %>%
  summarise(checkouts = sum(Checkouts)) %>%
  pull(checkouts)


#The total checkouts of Hemingway's book of the subject Fiction
Hemingway_Fiction <- np_Hemingway_data %>%
  select(year = CheckoutYear, Subjects = Subjects, Checkouts = Checkouts) %>%
  mutate(summarise_subjects = gsub("Classic Literature.*","Classic Literature", Subjects)) %>%
  mutate(summarise_subjects2 = gsub("Biography & Autobiography,.*","Biography & Autobiography", summarise_subjects)) %>%
  mutate(summarise_subjects3 = gsub("Fiction,.*","Fiction", summarise_subjects2)) %>%
  mutate(summarise_subjects4 = gsub("History,.*","History", summarise_subjects3)) %>%
  mutate(summarise_subjects5 = gsub("Literary Criticism,.*","Literary Criticism", summarise_subjects4)) %>%
  mutate(Subjects = gsub("Nonfiction,.*","Nonfiction", summarise_subjects5)) %>%
  group_by(Subjects) %>%
  filter(Subjects %in% c("Fiction")) %>%
  summarise(checkouts = sum(Checkouts)) %>%
  pull(checkouts)

#The total checkouts of Hemingway's book of the subject History
Hemingway_History <- np_Hemingway_data %>%
  select(year = CheckoutYear, Subjects = Subjects, Checkouts = Checkouts) %>%
  mutate(summarise_subjects = gsub("Classic Literature.*","Classic Literature", Subjects)) %>%
  mutate(summarise_subjects2 = gsub("Biography & Autobiography,.*","Biography & Autobiography", summarise_subjects)) %>%
  mutate(summarise_subjects3 = gsub("Fiction,.*","Fiction", summarise_subjects2)) %>%
  mutate(summarise_subjects4 = gsub("History,.*","History", summarise_subjects3)) %>%
  mutate(summarise_subjects5 = gsub("Literary Criticism,.*","Literary Criticism", summarise_subjects4)) %>%
  mutate(Subjects = gsub("Nonfiction,.*","Nonfiction", summarise_subjects5)) %>%
  group_by(Subjects) %>%
  filter(Subjects %in% c("History")) %>%
  summarise(checkouts = sum(Checkouts)) %>%
  pull(checkouts)


#The total checkouts of Hemingway's book of the subject Literary Criticism
Hemingway_Literary_Criticism <- np_Hemingway_data %>%
  select(year = CheckoutYear, Subjects = Subjects, Checkouts = Checkouts) %>%
  mutate(summarise_subjects = gsub("Classic Literature.*","Classic Literature", Subjects)) %>%
  mutate(summarise_subjects2 = gsub("Biography & Autobiography,.*","Biography & Autobiography", summarise_subjects)) %>%
  mutate(summarise_subjects3 = gsub("Fiction,.*","Fiction", summarise_subjects2)) %>%
  mutate(summarise_subjects4 = gsub("History,.*","History", summarise_subjects3)) %>%
  mutate(summarise_subjects5 = gsub("Literary Criticism,.*","Literary Criticism", summarise_subjects4)) %>%
  mutate(Subjects = gsub("Nonfiction,.*","Nonfiction", summarise_subjects5)) %>%
  group_by(Subjects) %>%
  filter(Subjects %in% c("Literary Criticism")) %>%
  summarise(checkouts = sum(Checkouts)) %>%
  pull(checkouts)


#The total checkouts of Hemingway's book of the subject Nonfiction
Hemingway_Nonfiction <- np_Hemingway_data %>%
  select(year = CheckoutYear, Subjects = Subjects, Checkouts = Checkouts) %>%
  mutate(summarise_subjects = gsub("Classic Literature.*","Classic Literature", Subjects)) %>%
  mutate(summarise_subjects2 = gsub("Biography & Autobiography,.*","Biography & Autobiography", summarise_subjects)) %>%
  mutate(summarise_subjects3 = gsub("Fiction,.*","Fiction", summarise_subjects2)) %>%
  mutate(summarise_subjects4 = gsub("History,.*","History", summarise_subjects3)) %>%
  mutate(summarise_subjects5 = gsub("Literary Criticism,.*","Literary Criticism", summarise_subjects4)) %>%
  mutate(Subjects = gsub("Nonfiction,.*","Nonfiction", summarise_subjects5)) %>%
  group_by(Subjects) %>%
  filter(Subjects %in% c("Nonfiction")) %>%
  summarise(checkouts = sum(Checkouts)) %>%
  pull(checkouts)


#The checkout percentage of Hemingway's book of each type
Hemingway_type_data <- np_Hemingway_data %>%
  group_by(type = MaterialType) %>%
  summarise(checkouts = sum(Checkouts))

total_checkouts = sum(book_data$checkouts, na.rm = TRUE)

Hemingway_type_data <- Hemingway_type_data %>%
  mutate(percentage_checkouts = (checkouts / total_checkouts))


#The checkout percentage of Hemingway's book of Audiobook
Hemingway_Audiobook <- Hemingway_type_data %>%
  filter(type %in% c("AUDIOBOOK")) %>%
  pull(percentage_checkouts)


#The checkout percentage of Hemingway's book of Ebook
Hemingway_Ebook <- Hemingway_type_data %>%
  filter(type %in% c("EBOOK")) %>%
  pull(percentage_checkouts)