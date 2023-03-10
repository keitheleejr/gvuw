setwd("/Users/keithlee/Documents/gvuw")

library(qualtRics)
library(Hmisc)
library(tidyverse)
library(gt)
library(gtExtras)

data <- read_survey("qualtrics-data.csv")

serv_need <- data |> 
  select(resources_non_access) |> 
  arrange(resources_non_access)

<<<<<<< HEAD
table <- as_tibble(unlist(strsplit(serv_need$resources_non_access,",")))

table |> 
  filter(!is.na(value),
         value != "None",
         value != "Other (please specify)") |> 
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated March 6, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")


# Happy

happy <- as.data.frame(table(data$happy)) %>% # create data frame
=======


data |> 
  group_by(county) |> 
  count() |>
  ggplot(aes(x = reorder(county,n), y = n, fill = county)) +
  geom_col() +
  coord_flip() +
  labs(y = "# of Responses",
       x = "County",
       title = "Number of Responses by County",
       caption = "Updated February 27, 2023") +
  geom_text(aes(label = n,
                hjust = 1.05)) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) 
  ggsave("response-by-county.png")

housing <- as.data.frame(table(data$housing)) %>% # create data frame
>>>>>>> e3eca85f13538c3485f40a195160cb0e307cd894
    mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) %>%
    arrange(desc(Percentage))



happy_table <- happy |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                           'Most of the time',
                                           'Some of the time',
                                           'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") %>%
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header(
    title = "I felt happy"
  )

happy_table


# sad

sad <- as.data.frame(table(data$sad)) %>% # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) %>%
  arrange(desc(Percentage))



sad_table <- sad |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") %>%
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header(
    title = "I felt sad"
  )

sad_table


# stressed

stressed <- as.data.frame(table(data$stressed)) %>% # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) %>%
  arrange(desc(Percentage))



stressed_table <- stressed |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") %>%
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header(
    title = "I felt stressed"
  )

stressed_table

# angry

angry <- as.data.frame(table(data$angry)) %>% # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) %>%
  arrange(desc(Percentage))



angry_table <- angry |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") %>%
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header(
    title = "I felt angry"
  )

angry_table

# frustrated

frustrated <- as.data.frame(table(data$frustrated)) %>% # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) %>%
  arrange(desc(Percentage))



frustrated_table <- frustrated |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") %>%
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header(
    title = "I felt frustrated"
  )

frustrated_table

# tired

tired <- as.data.frame(table(data$tired)) %>% # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) %>%
  arrange(desc(Percentage))



tired_table <- tired |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") %>%
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header(
    title = "I felt tired"
  )

tired_table

# hopeful

hopeful <- as.data.frame(table(data$hopeful)) %>% # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) %>%
  arrange(desc(Percentage))



hopeful_table <- hopeful |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") %>%
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header(
    title = "I felt hopeful"
  )

hopeful_table

# worried

worried <- as.data.frame(table(data$worried)) %>% # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) %>%
  arrange(desc(Percentage))



worried_table <- worried |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") %>%
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header(
    title = "I felt worried"
  )

worried_table


# View(data)
# 
# housing <- as.data.frame(table(data$housing)) %>% # create data frame
#     mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) %>%
#     arrange(desc(Percentage))
# 
# housing_table <- housing %>%
#     gt() %>%
#     cols_label(
#         Var1 = "Response",
#         Freq = "Count") %>%
#     cols_align(
#         align = "left",
#         columns = Var1
#         ) # add additional cols_align if need to align additional columns
# 
# housing_table
# 
# 
# household_income <- as.data.frame(table(data$household_income)) %>%
#     mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) %>%
#     arrange(desc(Percentage))
# 
# household_income_table <- household_income %>%
#     gt() %>%
#     cols_label(
#         Var1 = "Response",
#         Freq = "Count") %>%
#     cols_align(
#         align = "left",
#         columns = Var1
#         ) # add additional cols_align if need to align additional columns
# 
# household_income_table
# 
# 
# str(data$zip_code)
# data$zip_code_factor <- as.factor(data$zip_code)
# levels(data$zip_code_factor)

# Variables
# age
# zip_code
# county
# household_income
# single_parent
# occupation
# housing
# food
# housing_stress
# energy_stress
# food_stress
# trans_stress
# internet_access
# internet_devices
# child_care
# child_care_needs
# child_care_stress
# clothing_stress
# services
# organizations
# resources_non_access
# why_non_access
# happy
# angry
# stressed
# angry
# frustrated
# tired
# hopeful
# worried