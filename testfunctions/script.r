#### ALL COUNTIES ####

###### household_income ####

household_income <- as.data.frame(table(data$household_income)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Greater than $90,000', 
                                       '$70,001 - $90,000',
                                       '$50,001 - $70,000',
                                       '$30,001 - $50,000',
                                       '$0 - $30,000',
                                       'Prefer not to answer')))  |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("What was your household income for 2022?")


###### single_parent #####

single_parent <- as.data.frame(table(data$single_parent)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Do you live in a single parent 
           household?") |> 
  tab_options(table.align = "left") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### housing #####

housing <- as.data.frame(table(data$housing)) |> # create data frame
  filter(Var1 != "Other (please specify)") |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Which option best describes your 
           current housing situation?") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### food #####



food <- as.data.frame(table(data$food)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2))  |> 
  mutate(Ordered = fct_relevel(Var1, c('All of the time', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely',
                                       'Never'))) |> 
  arrange(Ordered) |>   
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("How often, if at all, is there enough food in 
           your household to feed all household members?")


###### internet_access #####

internet_access <- as.data.frame(table(data$internet_access)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Very reliable",
                                       "Somewhat reliable",
                                       "Not at all reliable",
                                       "Not applicable, I do not have internet access at home")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("At home, how reliable 
           is your internet access?" )

###### internet_devices #####



###### child_care #####

child_care <- as.data.frame(table(data$child_care)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Yes, my child care needs are met",
                                       "Yes, we have additional child care needs",
                                       "Yes, we require child care but do not have it",
                                       "Not applicable, my children are old enough they do not require childcare",
                                       "Not applicable, I do not have children")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Do you require childcare?")


###### child_care_needs #####

child_care_needs <- as.data.frame(table(data$child_care_needs)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Full-time child care",
                                       "After-school child care")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Which option best 
           describes your child care needs?")



###### services #####

services_bars <- as_tibble(unlist(strsplit(data$services,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")


###### organizations #####

org_bars <- as_tibble(unlist(strsplit(data$organizations,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### resources_non_access #####


rna_bars <- as_tibble(unlist(strsplit(data$resources_non_access,","))) |> 
  filter(!is.na(value),
         value != "None",
         value != "Other (please specify)",
         value != " I was able to access all the resources I needed") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### why_non_access  #####


wna <- as_tibble(unlist(strsplit(data$why_non_access,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> 
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Why were you unable to access resources?",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")



###### Happy #####

happy <- as.data.frame(table(data$happy)) |>  # create data frame 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |> 
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt happy since January 2022.")



###### sad #####

sad <- as.data.frame(table(data$sad)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt sad since January 2022.")



###### stressed #####

stressed <- as.data.frame(table(data$stressed)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt stressed since January 2022.")


###### angry #####

angry <- as.data.frame(table(data$angry)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt angry since January 2022.")


###### frustrated #####

frustrated <- as.data.frame(table(data$frustrated)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt frustrated since January 2022.")



###### tired #####

tired <- as.data.frame(table(data$tired)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt tired since January 2022.")


###### hopeful #####

hopeful <- as.data.frame(table(data$hopeful)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered)|> 
  tab_header("Please rate how often you have 
           felt hopeful since January 2022.")


###### worried #####

worried <- as.data.frame(table(data$worried)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt worried since January 2022.")



##### Atkinson #####

atkinson <- data |> 
  filter(county == "atkinson")

###### household_income ####

household_income_atkinson <- as.data.frame(table(atkinson$household_income)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Greater than $90,000', 
                                       '$70,001 - $90,000',
                                       '$50,001 - $70,000',
                                       '$30,001 - $50,000',
                                       '$0 - $30,000',
                                       'Prefer not to answer')))  |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("What was your household income for 2022?")


###### single_parent #####

single_parent_atkinson <- as.data.frame(table(atkinson$single_parent)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Do you live in a single parent 
             household?") |> 
  tab_options(table.align = "left") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### housing #####

housing_atkinson <- as.data.frame(table(atkinson$housing)) |> # create data frame
  filter(Var1 != "Other (please specify)") |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Which option best describes your 
             current housing situation?") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### food #####



food_atkinson <- as.data.frame(table(atkinson$food)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2))  |> 
  mutate(Ordered = fct_relevel(Var1, c('All of the time', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely',
                                       'Never'))) |> 
  arrange(Ordered) |>   
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("How often, if at all, is there enough food in 
             your household to feed all household members?")


###### internet_access #####

internet_access_atkinson <- as.data.frame(table(atkinson$internet_access)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Very reliable",
                                       "Somewhat reliable",
                                       "Not at all reliable",
                                       "Not applicable, I do not have internet access at home")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("At home, how reliable 
             is your internet access?" )

###### internet_devices #####



###### child_care #####

child_care_atkinson <- as.data.frame(table(atkinson$child_care)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Yes, my child care needs are met",
                                       "Yes, we have additional child care needs",
                                       "Yes, we require child care but do not have it",
                                       "Not applicable, my children are old enough they do not require childcare",
                                       "Not applicable, I do not have children")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Do you require childcare?")


###### child_care_needs #####

child_care_needs_atkinson <- as.data.frame(table(atkinson$child_care_needs)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Full-time child care",
                                       "After-school child care")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Which option best 
             describes your child care needs?")



###### services #####

services_plot_atkinson <- as_tibble(unlist(strsplit(atkinson$services,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")


###### organizations #####

org_atkinson <- as_tibble(unlist(strsplit(atkinson$organizations,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### resources_non_access #####


rna_atkinson <- as_tibble(unlist(strsplit(atkinson$resources_non_access,","))) |> 
  filter(!is.na(value),
         value != "None",
         value != "Other (please specify)",
         value != " I was able to access all the resources I needed") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### why_non_access  #####


wna_atkinson <- as_tibble(unlist(strsplit(atkinson$why_non_access,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> 
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Why were you unable to access resources?",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")



###### Happy #####

happy_atkinson <- as.data.frame(table(atkinson$happy)) |>  # create data frame 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |> 
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt happy since January 2022.")



###### sad #####

sad_atkinson <- as.data.frame(table(atkinson$sad)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt sad since January 2022.")



###### stressed #####

stressed_atkinson <- as.data.frame(table(atkinson$stressed)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt stressed since January 2022.")


###### angry #####

angry_atkinson <- as.data.frame(table(atkinson$angry)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt angry since January 2022.")


###### frustrated #####

frustrated_atkinson <- as.data.frame(table(atkinson$frustrated)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt frustrated since January 2022.")



###### tired #####

tired_atkinson <- as.data.frame(table(atkinson$tired)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt tired since January 2022.")


###### hopeful #####

hopeful_atkinson <- as.data.frame(table(atkinson$hopeful)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered)|> 
  tab_header("Please rate how often you have 
             felt hopeful since January 2022.")


###### worried #####

worried_atkinson <- as.data.frame(table(atkinson$worried)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt worried since January 2022.")


##### Bacon ###### 


bacon <- data |> 
  filter(county == "bacon")

###### household_income ####

household_income_bacon <- as.data.frame(table(bacon$household_income)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Greater than $90,000', 
                                       '$70,001 - $90,000',
                                       '$50,001 - $70,000',
                                       '$30,001 - $50,000',
                                       '$0 - $30,000',
                                       'Prefer not to answer')))  |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("What was your household income for 2022?")


###### single_parent #####

single_parent_bacon <- as.data.frame(table(bacon$single_parent)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Do you live in a single parent 
             household?") |> 
  tab_options(table.align = "left") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### housing #####

housing_bacon <- as.data.frame(table(bacon$housing)) |> # create data frame
  filter(Var1 != "Other (please specify)") |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Which option best describes your 
             current housing situation?") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### food #####



food_bacon <- as.data.frame(table(bacon$food)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2))  |> 
  mutate(Ordered = fct_relevel(Var1, c('All of the time', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely',
                                       'Never'))) |> 
  arrange(Ordered) |>   
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("How often, if at all, is there enough food in 
             your household to feed all household members?")


###### internet_access #####

internet_access_bacon <- as.data.frame(table(bacon$internet_access)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Very reliable",
                                       "Somewhat reliable",
                                       "Not at all reliable",
                                       "Not applicable, I do not have internet access at home")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("At home, how reliable 
             is your internet access?" )

###### internet_devices #####



###### child_care #####

child_care_bacon <- as.data.frame(table(bacon$child_care)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Yes, my child care needs are met",
                                       "Yes, we have additional child care needs",
                                       "Yes, we require child care but do not have it",
                                       "Not applicable, my children are old enough they do not require childcare",
                                       "Not applicable, I do not have children")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Do you require childcare?")


###### child_care_needs #####

child_care_needs_bacon <- as.data.frame(table(bacon$child_care_needs)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Full-time child care",
                                       "After-school child care")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Which option best 
             describes your child care needs?")



###### services #####

services_plot_bacon <- as_tibble(unlist(strsplit(bacon$services,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")


###### organizations #####

org_bacon <- as_tibble(unlist(strsplit(bacon$organizations,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### resources_non_access #####


rna_bacon <- as_tibble(unlist(strsplit(bacon$resources_non_access,","))) |> 
  filter(!is.na(value),
         value != "None",
         value != "Other (please specify)",
         value != " I was able to access all the resources I needed") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### why_non_access  #####


wna_bacon <- as_tibble(unlist(strsplit(bacon$why_non_access,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> 
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Why were you unable to access resources?",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")



###### Happy #####

happy_bacon <- as.data.frame(table(bacon$happy)) |>  # create data frame 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |> 
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt happy since January 2022.")



###### sad #####

sad_bacon <- as.data.frame(table(bacon$sad)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt sad since January 2022.")



###### stressed #####

stressed_bacon <- as.data.frame(table(bacon$stressed)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt stressed since January 2022.")


###### angry #####

angry_bacon <- as.data.frame(table(bacon$angry)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt angry since January 2022.")


###### frustrated #####

frustrated_bacon <- as.data.frame(table(bacon$frustrated)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt frustrated since January 2022.")



###### tired #####

tired_bacon <- as.data.frame(table(bacon$tired)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt tired since January 2022.")


###### hopeful #####

hopeful_bacon <- as.data.frame(table(bacon$hopeful)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered)|> 
  tab_header("Please rate how often you have 
             felt hopeful since January 2022.")


###### worried #####

worried_bacon <- as.data.frame(table(bacon$worried)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt worried since January 2022.")


#### Berrien ####

berrien <- data |> 
  filter(county == "berrien")

###### household_income ####

household_income_berrien <- as.data.frame(table(berrien$household_income)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Greater than $90,000', 
                                       '$70,001 - $90,000',
                                       '$50,001 - $70,000',
                                       '$30,001 - $50,000',
                                       '$0 - $30,000',
                                       'Prefer not to answer')))  |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("What was your household income for 2022?")


###### single_parent #####

single_parent_berrien <- as.data.frame(table(berrien$single_parent)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Do you live in a single parent 
             household?") |> 
  tab_options(table.align = "left") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### housing #####

housing_berrien <- as.data.frame(table(berrien$housing)) |> # create data frame
  filter(Var1 != "Other (please specify)") |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Which option best describes your 
             current housing situation?") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### food #####



food_berrien <- as.data.frame(table(berrien$food)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2))  |> 
  mutate(Ordered = fct_relevel(Var1, c('All of the time', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely',
                                       'Never'))) |> 
  arrange(Ordered) |>   
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("How often, if at all, is there enough food in 
             your household to feed all household members?")


###### internet_access #####

internet_access_berrien <- as.data.frame(table(berrien$internet_access)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Very reliable",
                                       "Somewhat reliable",
                                       "Not at all reliable",
                                       "Not applicable, I do not have internet access at home")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("At home, how reliable 
             is your internet access?" )

###### internet_devices #####



###### child_care #####

child_care_berrien <- as.data.frame(table(berrien$child_care)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Yes, my child care needs are met",
                                       "Yes, we have additional child care needs",
                                       "Yes, we require child care but do not have it",
                                       "Not applicable, my children are old enough they do not require childcare",
                                       "Not applicable, I do not have children")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Do you require childcare?")


###### child_care_needs #####

child_care_needs_berrien <- as.data.frame(table(berrien$child_care_needs)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Full-time child care",
                                       "After-school child care")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Which option best 
             describes your child care needs?")



###### services #####

services_plot_berrien <- as_tibble(unlist(strsplit(berrien$services,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")


###### organizations #####

org_berrien <- as_tibble(unlist(strsplit(berrien$organizations,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### resources_non_access #####


rna_berrien <- as_tibble(unlist(strsplit(berrien$resources_non_access,","))) |> 
  filter(!is.na(value),
         value != "None",
         value != "Other (please specify)",
         value != " I was able to access all the resources I needed") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### why_non_access  #####


wna_berrien <- as_tibble(unlist(strsplit(berrien$why_non_access,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> 
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Why were you unable to access resources?",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")



###### Happy #####

happy_berrien <- as.data.frame(table(berrien$happy)) |>  # create data frame 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |> 
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt happy since January 2022.")



###### sad #####

sad_berrien <- as.data.frame(table(berrien$sad)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt sad since January 2022.")



###### stressed #####

stressed_berrien <- as.data.frame(table(berrien$stressed)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt stressed since January 2022.")


###### angry #####

angry_berrien <- as.data.frame(table(berrien$angry)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt angry since January 2022.")


###### frustrated #####

frustrated_berrien <- as.data.frame(table(berrien$frustrated)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt frustrated since January 2022.")



###### tired #####

tired_berrien <- as.data.frame(table(berrien$tired)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt tired since January 2022.")


###### hopeful #####

hopeful_berrien <- as.data.frame(table(berrien$hopeful)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered)|> 
  tab_header("Please rate how often you have 
             felt hopeful since January 2022.")


###### worried #####

worried_berrien <- as.data.frame(table(berrien$worried)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt worried since January 2022.")

##### Brooks ######

brooks <- data |> 
  filter(county == "brooks")

###### household_income ####

household_income_brooks <- as.data.frame(table(brooks$household_income)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Greater than $90,000', 
                                       '$70,001 - $90,000',
                                       '$50,001 - $70,000',
                                       '$30,001 - $50,000',
                                       '$0 - $30,000',
                                       'Prefer not to answer')))  |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("What was your household income for 2022?")


###### single_parent #####

single_parent_brooks <- as.data.frame(table(brooks$single_parent)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Do you live in a single parent 
             household?") |> 
  tab_options(table.align = "left") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### housing #####

housing_brooks <- as.data.frame(table(brooks$housing)) |> # create data frame
  filter(Var1 != "Other (please specify)") |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Which option best describes your 
             current housing situation?") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### food #####



food_brooks <- as.data.frame(table(brooks$food)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2))  |> 
  mutate(Ordered = fct_relevel(Var1, c('All of the time', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely',
                                       'Never'))) |> 
  arrange(Ordered) |>   
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("How often, if at all, is there enough food in 
             your household to feed all household members?")


###### internet_access #####

internet_access_brooks <- as.data.frame(table(brooks$internet_access)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Very reliable",
                                       "Somewhat reliable",
                                       "Not at all reliable",
                                       "Not applicable, I do not have internet access at home")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("At home, how reliable 
             is your internet access?" )

###### internet_devices #####



###### child_care #####

child_care_brooks <- as.data.frame(table(brooks$child_care)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Yes, my child care needs are met",
                                       "Yes, we have additional child care needs",
                                       "Yes, we require child care but do not have it",
                                       "Not applicable, my children are old enough they do not require childcare",
                                       "Not applicable, I do not have children")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Do you require childcare?")


###### child_care_needs #####

child_care_needs_brooks <- as.data.frame(table(brooks$child_care_needs)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Full-time child care",
                                       "After-school child care")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Which option best 
             describes your child care needs?")



###### services #####

services_plot_brooks <- as_tibble(unlist(strsplit(brooks$services,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")


###### organizations #####

org_brooks <- as_tibble(unlist(strsplit(brooks$organizations,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### resources_non_access #####


rna_brooks <- as_tibble(unlist(strsplit(brooks$resources_non_access,","))) |> 
  filter(!is.na(value),
         value != "None",
         value != "Other (please specify)",
         value != " I was able to access all the resources I needed") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### why_non_access  #####


wna_brooks <- as_tibble(unlist(strsplit(brooks$why_non_access,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> 
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Why were you unable to access resources?",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")



###### Happy #####

happy_brooks <- as.data.frame(table(brooks$happy)) |>  # create data frame 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |> 
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt happy since January 2022.")



###### sad #####

sad_brooks <- as.data.frame(table(brooks$sad)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt sad since January 2022.")



###### stressed #####

stressed_brooks <- as.data.frame(table(brooks$stressed)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt stressed since January 2022.")


###### angry #####

angry_brooks <- as.data.frame(table(brooks$angry)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt angry since January 2022.")


###### frustrated #####

frustrated_brooks <- as.data.frame(table(brooks$frustrated)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt frustrated since January 2022.")



###### tired #####

tired_brooks <- as.data.frame(table(brooks$tired)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt tired since January 2022.")


###### hopeful #####

hopeful_brooks <- as.data.frame(table(brooks$hopeful)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered)|> 
  tab_header("Please rate how often you have 
             felt hopeful since January 2022.")


###### worried #####

worried_brooks <- as.data.frame(table(brooks$worried)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt worried since January 2022.")

##### Clinch #####

clinch <- data |> 
  filter(county == "clinch")

###### household_income ####

household_income_clinch <- as.data.frame(table(clinch$household_income)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Greater than $90,000', 
                                       '$70,001 - $90,000',
                                       '$50,001 - $70,000',
                                       '$30,001 - $50,000',
                                       '$0 - $30,000',
                                       'Prefer not to answer')))  |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("What was your household income for 2022?")


###### single_parent #####

single_parent_clinch <- as.data.frame(table(clinch$single_parent)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Do you live in a single parent 
             household?") |> 
  tab_options(table.align = "left") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### housing #####

housing_clinch <- as.data.frame(table(clinch$housing)) |> # create data frame
  filter(Var1 != "Other (please specify)") |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Which option best describes your 
             current housing situation?") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### food #####



food_clinch <- as.data.frame(table(clinch$food)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2))  |> 
  mutate(Ordered = fct_relevel(Var1, c('All of the time', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely',
                                       'Never'))) |> 
  arrange(Ordered) |>   
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("How often, if at all, is there enough food in 
             your household to feed all household members?")


###### internet_access #####

internet_access_clinch <- as.data.frame(table(clinch$internet_access)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Very reliable",
                                       "Somewhat reliable",
                                       "Not at all reliable",
                                       "Not applicable, I do not have internet access at home")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("At home, how reliable 
             is your internet access?" )

###### internet_devices #####



###### child_care #####

child_care_clinch <- as.data.frame(table(clinch$child_care)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Yes, my child care needs are met",
                                       "Yes, we have additional child care needs",
                                       "Yes, we require child care but do not have it",
                                       "Not applicable, my children are old enough they do not require childcare",
                                       "Not applicable, I do not have children")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Do you require childcare?")


###### child_care_needs #####

child_care_needs_clinch <- as.data.frame(table(clinch$child_care_needs)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Full-time child care",
                                       "After-school child care")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Which option best 
             describes your child care needs?")



###### services #####

services_plot_clinch <- as_tibble(unlist(strsplit(clinch$services,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")


###### organizations #####

org_clinch <- as_tibble(unlist(strsplit(clinch$organizations,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### resources_non_access #####


rna_clinch <- as_tibble(unlist(strsplit(clinch$resources_non_access,","))) |> 
  filter(!is.na(value),
         value != "None",
         value != "Other (please specify)",
         value != " I was able to access all the resources I needed") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### why_non_access  #####


wna_clinch <- as_tibble(unlist(strsplit(clinch$why_non_access,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> 
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Why were you unable to access resources?",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")



###### Happy #####

happy_clinch <- as.data.frame(table(clinch$happy)) |>  # create data frame 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |> 
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt happy since January 2022.")



###### sad #####

sad_clinch <- as.data.frame(table(clinch$sad)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt sad since January 2022.")



###### stressed #####

stressed_clinch <- as.data.frame(table(clinch$stressed)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt stressed since January 2022.")


###### angry #####

angry_clinch <- as.data.frame(table(clinch$angry)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt angry since January 2022.")


###### frustrated #####

frustrated_clinch <- as.data.frame(table(clinch$frustrated)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt frustrated since January 2022.")



###### tired #####

tired_clinch <- as.data.frame(table(clinch$tired)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt tired since January 2022.")


###### hopeful #####

hopeful_clinch <- as.data.frame(table(clinch$hopeful)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered)|> 
  tab_header("Please rate how often you have 
             felt hopeful since January 2022.")


###### worried #####

worried_clinch <- as.data.frame(table(clinch$worried)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt worried since January 2022.")


##### Coffee ######

coffee <- data |> 
  filter(county == "coffee")

###### household_income ####

household_income_coffee <- as.data.frame(table(coffee$household_income)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Greater than $90,000', 
                                       '$70,001 - $90,000',
                                       '$50,001 - $70,000',
                                       '$30,001 - $50,000',
                                       '$0 - $30,000',
                                       'Prefer not to answer')))  |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("What was your household income for 2022?")


###### single_parent #####

single_parent_coffee <- as.data.frame(table(coffee$single_parent)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Do you live in a single parent 
           household?") |> 
  tab_options(table.align = "left") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### housing #####

housing_coffee <- as.data.frame(table(coffee$housing)) |> # create data frame
  filter(Var1 != "Other (please specify)") |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Which option best describes your 
           current housing situation?") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### food #####



food_coffee <- as.data.frame(table(coffee$food)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2))  |> 
  mutate(Ordered = fct_relevel(Var1, c('All of the time', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely',
                                       'Never'))) |> 
  arrange(Ordered) |>   
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("How often, if at all, is there enough food in 
           your household to feed all household members?")


###### internet_access #####

internet_access_coffee <- as.data.frame(table(coffee$internet_access)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Very reliable",
                                       "Somewhat reliable",
                                       "Not at all reliable",
                                       "Not applicable, I do not have internet access at home")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("At home, how reliable 
           is your internet access?" )

###### internet_devices #####



###### child_care #####

child_care_coffee <- as.data.frame(table(coffee$child_care)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Yes, my child care needs are met",
                                       "Yes, we have additional child care needs",
                                       "Yes, we require child care but do not have it",
                                       "Not applicable, my children are old enough they do not require childcare",
                                       "Not applicable, I do not have children")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Do you require childcare?")


###### child_care_needs #####

child_care_needs_coffee <- as.data.frame(table(coffee$child_care_needs)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Full-time child care",
                                       "After-school child care")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Which option best 
           describes your child care needs?")



###### services #####

services_plot_coffee <- as_tibble(unlist(strsplit(coffee$services,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")


###### organizations #####

org_coffee <- as_tibble(unlist(strsplit(coffee$organizations,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### resources_non_access #####


rna_coffee <- as_tibble(unlist(strsplit(coffee$resources_non_access,","))) |> 
  filter(!is.na(value),
         value != "None",
         value != "Other (please specify)",
         value != " I was able to access all the resources I needed") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### why_non_access  #####


wna_coffee <- as_tibble(unlist(strsplit(coffee$why_non_access,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> 
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Why were you unable to access resources?",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")



###### Happy #####

happy_coffee <- as.data.frame(table(coffee$happy)) |>  # create data frame 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |> 
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt happy since January 2022.")



###### sad #####

sad_coffee <- as.data.frame(table(coffee$sad)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt sad since January 2022.")



###### stressed #####

stressed_coffee <- as.data.frame(table(coffee$stressed)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt stressed since January 2022.")


###### angry #####

angry_coffee <- as.data.frame(table(coffee$angry)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt angry since January 2022.")


###### frustrated #####

frustrated_coffee <- as.data.frame(table(coffee$frustrated)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt frustrated since January 2022.")



###### tired #####

tired_coffee <- as.data.frame(table(coffee$tired)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt tired since January 2022.")


###### hopeful #####

hopeful_coffee <- as.data.frame(table(coffee$hopeful)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered)|> 
  tab_header("Please rate how often you have 
           felt hopeful since January 2022.")


###### worried #####

worried_coffee <- as.data.frame(table(coffee$worried)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt worried since January 2022.")

##### Echols ######

echols <- data |> 
  filter(county == "echols")

###### household_income ####

household_income_echols <- as.data.frame(table(echols$household_income)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Greater than $90,000', 
                                       '$70,001 - $90,000',
                                       '$50,001 - $70,000',
                                       '$30,001 - $50,000',
                                       '$0 - $30,000',
                                       'Prefer not to answer')))  |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("What was your household income for 2022?")


###### single_parent #####

single_parent_echols <- as.data.frame(table(echols$single_parent)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Do you live in a single parent 
           household?") |> 
  tab_options(table.align = "left") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### housing #####

housing_echols <- as.data.frame(table(echols$housing)) |> # create data frame
  filter(Var1 != "Other (please specify)") |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Which option best describes your 
           current housing situation?") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### food #####



food_echols <- as.data.frame(table(echols$food)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2))  |> 
  mutate(Ordered = fct_relevel(Var1, c('All of the time', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely',
                                       'Never'))) |> 
  arrange(Ordered) |>   
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("How often, if at all, is there enough food in 
           your household to feed all household members?")


###### internet_access #####

internet_access_echols <- as.data.frame(table(echols$internet_access)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Very reliable",
                                       "Somewhat reliable",
                                       "Not at all reliable",
                                       "Not applicable, I do not have internet access at home")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("At home, how reliable 
           is your internet access?" )

###### internet_devices #####



###### child_care #####

child_care_echols <- as.data.frame(table(echols$child_care)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Yes, my child care needs are met",
                                       "Yes, we have additional child care needs",
                                       "Yes, we require child care but do not have it",
                                       "Not applicable, my children are old enough they do not require childcare",
                                       "Not applicable, I do not have children")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Do you require childcare?")


###### child_care_needs #####

child_care_needs_echols <- as.data.frame(table(echols$child_care_needs)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Full-time child care",
                                       "After-school child care")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Which option best 
           describes your child care needs?")



###### services #####

services_plot_echols <- as_tibble(unlist(strsplit(echols$services,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")


###### organizations #####

org_echols <- as_tibble(unlist(strsplit(echols$organizations,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### resources_non_access #####


rna_echols <- as_tibble(unlist(strsplit(echols$resources_non_access,","))) |> 
  filter(!is.na(value),
         value != "None",
         value != "Other (please specify)",
         value != " I was able to access all the resources I needed") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### why_non_access  #####


wna_echols <- as_tibble(unlist(strsplit(echols$why_non_access,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> 
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Why were you unable to access resources?",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")



###### Happy #####

happy_echols <- as.data.frame(table(echols$happy)) |>  # create data frame 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |> 
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt happy since January 2022.")



###### sad #####

sad_echols <- as.data.frame(table(echols$sad)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt sad since January 2022.")



###### stressed #####

stressed_echols <- as.data.frame(table(echols$stressed)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt stressed since January 2022.")


###### angry #####

angry_echols <- as.data.frame(table(echols$angry)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt angry since January 2022.")


###### frustrated #####

frustrated_echols <- as.data.frame(table(echols$frustrated)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt frustrated since January 2022.")



###### tired #####

tired_echols <- as.data.frame(table(echols$tired)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt tired since January 2022.")


###### hopeful #####

hopeful_echols <- as.data.frame(table(echols$hopeful)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered)|> 
  tab_header("Please rate how often you have 
           felt hopeful since January 2022.")


###### worried #####

worried_echols <- as.data.frame(table(echols$worried)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt worried since January 2022.")


##### Jeff Davis ######

jeffdavis <- data |> 
  filter(county == "jeff davis")

###### household_income ####

household_income_jeffdavis <- as.data.frame(table(jeffdavis$household_income)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Greater than $90,000', 
                                       '$70,001 - $90,000',
                                       '$50,001 - $70,000',
                                       '$30,001 - $50,000',
                                       '$0 - $30,000',
                                       'Prefer not to answer')))  |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("What was your household income for 2022?")


###### single_parent #####

single_parent_jeffdavis <- as.data.frame(table(jeffdavis$single_parent)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Do you live in a single parent 
           household?") |> 
  tab_options(table.align = "left") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### housing #####

housing_jeffdavis <- as.data.frame(table(jeffdavis$housing)) |> # create data frame
  filter(Var1 != "Other (please specify)") |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Which option best describes your 
           current housing situation?") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### food #####



food_jeffdavis <- as.data.frame(table(jeffdavis$food)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2))  |> 
  mutate(Ordered = fct_relevel(Var1, c('All of the time', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely',
                                       'Never'))) |> 
  arrange(Ordered) |>   
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("How often, if at all, is there enough food in 
           your household to feed all household members?")


###### internet_access #####

internet_access_jeffdavis <- as.data.frame(table(jeffdavis$internet_access)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Very reliable",
                                       "Somewhat reliable",
                                       "Not at all reliable",
                                       "Not applicable, I do not have internet access at home")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("At home, how reliable 
           is your internet access?" )

###### internet_devices #####



###### child_care #####

child_care_jeffdavis <- as.data.frame(table(jeffdavis$child_care)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Yes, my child care needs are met",
                                       "Yes, we have additional child care needs",
                                       "Yes, we require child care but do not have it",
                                       "Not applicable, my children are old enough they do not require childcare",
                                       "Not applicable, I do not have children")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Do you require childcare?")


###### child_care_needs #####

child_care_needs_jeffdavis <- as.data.frame(table(jeffdavis$child_care_needs)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Full-time child care",
                                       "After-school child care")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Which option best 
           describes your child care needs?")



###### services #####

services_plot_jeffdavis <- as_tibble(unlist(strsplit(jeffdavis$services,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")


###### organizations #####

org_jeffdavis <- as_tibble(unlist(strsplit(jeffdavis$organizations,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### resources_non_access #####


rna_jeffdavis <- as_tibble(unlist(strsplit(jeffdavis$resources_non_access,","))) |> 
  filter(!is.na(value),
         value != "None",
         value != "Other (please specify)",
         value != " I was able to access all the resources I needed") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### why_non_access  #####


wna_jeffdavis <- as_tibble(unlist(strsplit(jeffdavis$why_non_access,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> 
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Why were you unable to access resources?",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")



###### Happy #####

happy_jeffdavis <- as.data.frame(table(jeffdavis$happy)) |>  # create data frame 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |> 
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt happy since January 2022.")



###### sad #####

sad_jeffdavis <- as.data.frame(table(jeffdavis$sad)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt sad since January 2022.")



###### stressed #####

stressed_jeffdavis <- as.data.frame(table(jeffdavis$stressed)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt stressed since January 2022.")


###### angry #####

angry_jeffdavis <- as.data.frame(table(jeffdavis$angry)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt angry since January 2022.")


###### frustrated #####

frustrated_jeffdavis <- as.data.frame(table(jeffdavis$frustrated)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt frustrated since January 2022.")



###### tired #####

tired_jeffdavis <- as.data.frame(table(jeffdavis$tired)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt tired since January 2022.")


###### hopeful #####

hopeful_jeffdavis <- as.data.frame(table(jeffdavis$hopeful)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered)|> 
  tab_header("Please rate how often you have 
           felt hopeful since January 2022.")


###### worried #####

worried_jeffdavis <- as.data.frame(table(jeffdavis$worried)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt worried since January 2022.")


##### Lanier ######

lanier <- data |> 
  filter(county == "lanier")

###### household_income ####

household_income_lanier <- as.data.frame(table(lanier$household_income)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Greater than $90,000', 
                                       '$70,001 - $90,000',
                                       '$50,001 - $70,000',
                                       '$30,001 - $50,000',
                                       '$0 - $30,000',
                                       'Prefer not to answer')))  |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("What was your household income for 2022?")


###### single_parent #####

single_parent_lanier <- as.data.frame(table(lanier$single_parent)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Do you live in a single parent 
           household?") |> 
  tab_options(table.align = "left") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### housing #####

housing_lanier <- as.data.frame(table(lanier$housing)) |> # create data frame
  filter(Var1 != "Other (please specify)") |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Which option best describes your 
           current housing situation?") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### food #####



food_lanier <- as.data.frame(table(lanier$food)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2))  |> 
  mutate(Ordered = fct_relevel(Var1, c('All of the time', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely',
                                       'Never'))) |> 
  arrange(Ordered) |>   
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("How often, if at all, is there enough food in 
           your household to feed all household members?")


###### internet_access #####

internet_access_lanier <- as.data.frame(table(lanier$internet_access)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Very reliable",
                                       "Somewhat reliable",
                                       "Not at all reliable",
                                       "Not applicable, I do not have internet access at home")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("At home, how reliable 
           is your internet access?" )

###### internet_devices #####



###### child_care #####

child_care_lanier <- as.data.frame(table(lanier$child_care)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Yes, my child care needs are met",
                                       "Yes, we have additional child care needs",
                                       "Yes, we require child care but do not have it",
                                       "Not applicable, my children are old enough they do not require childcare",
                                       "Not applicable, I do not have children")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Do you require childcare?")


###### child_care_needs #####

child_care_needs_lanier <- as.data.frame(table(lanier$child_care_needs)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Full-time child care",
                                       "After-school child care")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Which option best 
           describes your child care needs?")



###### services #####

services_plot_lanier <- as_tibble(unlist(strsplit(lanier$services,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")


###### organizations #####

org_lanier <- as_tibble(unlist(strsplit(lanier$organizations,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### resources_non_access #####


rna_lanier <- as_tibble(unlist(strsplit(lanier$resources_non_access,","))) |> 
  filter(!is.na(value),
         value != "None",
         value != "Other (please specify)",
         value != " I was able to access all the resources I needed") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### why_non_access  #####


wna_lanier <- as_tibble(unlist(strsplit(lanier$why_non_access,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> 
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Why were you unable to access resources?",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")



###### Happy #####

happy_lanier <- as.data.frame(table(lanier$happy)) |>  # create data frame 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |> 
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt happy since January 2022.")



###### sad #####

sad_lanier <- as.data.frame(table(lanier$sad)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt sad since January 2022.")



###### stressed #####

stressed_lanier <- as.data.frame(table(lanier$stressed)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt stressed since January 2022.")


###### angry #####

angry_lanier <- as.data.frame(table(lanier$angry)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt angry since January 2022.")


###### frustrated #####

frustrated_lanier <- as.data.frame(table(lanier$frustrated)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt frustrated since January 2022.")



###### tired #####

tired_lanier <- as.data.frame(table(lanier$tired)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt tired since January 2022.")


###### hopeful #####

hopeful_lanier <- as.data.frame(table(lanier$hopeful)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered)|> 
  tab_header("Please rate how often you have 
           felt hopeful since January 2022.")


###### worried #####

worried_lanier <- as.data.frame(table(lanier$worried)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt worried since January 2022.")

##### Lowndes ######

lowndes <- data |> 
  filter(county == "lowndes")

###### household_income ####

household_income_lowndes <- as.data.frame(table(lowndes$household_income)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Greater than $90,000', 
                                       '$70,001 - $90,000',
                                       '$50,001 - $70,000',
                                       '$30,001 - $50,000',
                                       '$0 - $30,000',
                                       'Prefer not to answer')))  |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("What was your household income for 2022?")


###### single_parent #####

single_parent_lowndes <- as.data.frame(table(lowndes$single_parent)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Do you live in a single parent 
           household?") |> 
  tab_options(table.align = "left") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### housing #####

housing_lowndes <- as.data.frame(table(lowndes$housing)) |> # create data frame
  filter(Var1 != "Other (please specify)") |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Which option best describes your 
           current housing situation?") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### food #####



food_lowndes <- as.data.frame(table(lowndes$food)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2))  |> 
  mutate(Ordered = fct_relevel(Var1, c('All of the time', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely',
                                       'Never'))) |> 
  arrange(Ordered) |>   
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("How often, if at all, is there enough food in 
           your household to feed all household members?")


###### internet_access #####

internet_access_lowndes <- as.data.frame(table(lowndes$internet_access)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Very reliable",
                                       "Somewhat reliable",
                                       "Not at all reliable",
                                       "Not applicable, I do not have internet access at home")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("At home, how reliable 
           is your internet access?" )

###### internet_devices #####



###### child_care #####

child_care_lowndes <- as.data.frame(table(lowndes$child_care)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Yes, my child care needs are met",
                                       "Yes, we have additional child care needs",
                                       "Yes, we require child care but do not have it",
                                       "Not applicable, my children are old enough they do not require childcare",
                                       "Not applicable, I do not have children")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Do you require childcare?")


###### child_care_needs #####

child_care_needs_lowndes <- as.data.frame(table(lowndes$child_care_needs)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Full-time child care",
                                       "After-school child care")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Which option best 
           describes your child care needs?")



###### services #####

services_plot_lowndes <- as_tibble(unlist(strsplit(lowndes$services,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")


###### organizations #####

org_lowndes <- as_tibble(unlist(strsplit(lowndes$organizations,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### resources_non_access #####


rna_lowndes <- as_tibble(unlist(strsplit(lowndes$resources_non_access,","))) |> 
  filter(!is.na(value),
         value != "None",
         value != "Other (please specify)",
         value != " I was able to access all the resources I needed") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### why_non_access  #####


wna_lowndes <- as_tibble(unlist(strsplit(lowndes$why_non_access,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> 
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Why were you unable to access resources?",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")



###### Happy #####

happy_lowndes <- as.data.frame(table(lowndes$happy)) |>  # create data frame 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |> 
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt happy since January 2022.")



###### sad #####

sad_lowndes <- as.data.frame(table(lowndes$sad)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt sad since January 2022.")



###### stressed #####

stressed_lowndes <- as.data.frame(table(lowndes$stressed)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt stressed since January 2022.")


###### angry #####

angry_lowndes <- as.data.frame(table(lowndes$angry)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt angry since January 2022.")


###### frustrated #####

frustrated_lowndes <- as.data.frame(table(lowndes$frustrated)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt frustrated since January 2022.")



###### tired #####

tired_lowndes <- as.data.frame(table(lowndes$tired)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt tired since January 2022.")


###### hopeful #####

hopeful_lowndes <- as.data.frame(table(lowndes$hopeful)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered)|> 
  tab_header("Please rate how often you have 
           felt hopeful since January 2022.")


###### worried #####

worried_lowndes <- as.data.frame(table(lowndes$worried)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt worried since January 2022.")


##### Ware ######

ware <- data |> 
  filter(county == "ware")

###### household_income ####

household_income_ware <- as.data.frame(table(ware$household_income)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Greater than $90,000', 
                                       '$70,001 - $90,000',
                                       '$50,001 - $70,000',
                                       '$30,001 - $50,000',
                                       '$0 - $30,000',
                                       'Prefer not to answer')))  |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("What was your household income for 2022?")


###### single_parent #####

single_parent_ware <- as.data.frame(table(ware$single_parent)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Do you live in a single parent 
           household?") |> 
  tab_options(table.align = "left") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### housing #####

housing_ware <- as.data.frame(table(ware$housing)) |> # create data frame
  filter(Var1 != "Other (please specify)") |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Which option best describes your 
           current housing situation?") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns



###### food #####



food_ware <- as.data.frame(table(ware$food)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2))  |> 
  mutate(Ordered = fct_relevel(Var1, c('All of the time', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely',
                                       'Never'))) |> 
  arrange(Ordered) |>   
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("How often, if at all, is there enough food in 
           your household to feed all household members?")


###### internet_access #####

internet_access_ware <- as.data.frame(table(ware$internet_access)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Very reliable",
                                       "Somewhat reliable",
                                       "Not at all reliable",
                                       "Not applicable, I do not have internet access at home")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("At home, how reliable 
           is your internet access?" )

###### internet_devices #####



###### child_care #####

child_care_ware <- as.data.frame(table(ware$child_care)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Yes, my child care needs are met",
                                       "Yes, we have additional child care needs",
                                       "Yes, we require child care but do not have it",
                                       "Not applicable, my children are old enough they do not require childcare",
                                       "Not applicable, I do not have children")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Do you require childcare?")


###### child_care_needs #####

child_care_needs_ware <- as.data.frame(table(ware$child_care_needs)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) |> 
  mutate(Ordered = fct_relevel(Var1, c("Full-time child care",
                                       "After-school child care")
  )
  ) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Which option best 
           describes your child care needs?")



###### services #####

services_plot_ware <- as_tibble(unlist(strsplit(ware$services,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")


###### organizations #####

org_ware <- as_tibble(unlist(strsplit(ware$organizations,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### resources_non_access #####


rna_ware <- as_tibble(unlist(strsplit(ware$resources_non_access,","))) |> 
  filter(!is.na(value),
         value != "None",
         value != "Other (please specify)",
         value != " I was able to access all the resources I needed") |> # learn how to remove the space here
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Resources Needed",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")

###### why_non_access  #####


wna_ware <- as_tibble(unlist(strsplit(ware$why_non_access,","))) |> 
  filter(!is.na(value),
         value != "Other (please specify)") |> 
  group_by(value) |> 
  count() |> 
  ggplot(aes(x = reorder(value,n), y = n, fill=value)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "Why were you unable to access resources?",
       y = "Number of Respondents",
       caption = "Updated April 28, 2023") +
  geom_text(aes(label = n,
                hjust = 1.25)) + 
  theme_classic() + 
  theme(legend.position = "none")



###### Happy #####

happy_ware <- as.data.frame(table(ware$happy)) |>  # create data frame 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |> 
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt happy since January 2022.")



###### sad #####

sad_ware <- as.data.frame(table(ware$sad)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt sad since January 2022.")



###### stressed #####

stressed_ware <- as.data.frame(table(ware$stressed)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt stressed since January 2022.")


###### angry #####

angry_ware <- as.data.frame(table(ware$angry)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt angry since January 2022.")


###### frustrated #####

frustrated_ware <- as.data.frame(table(ware$frustrated)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt frustrated since January 2022.")



###### tired #####

tired_ware <- as.data.frame(table(ware$tired)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt tired since January 2022.")


###### hopeful #####

hopeful_ware <- as.data.frame(table(ware$hopeful)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered)|> 
  tab_header("Please rate how often you have 
           felt hopeful since January 2022.")


###### worried #####

worried_ware <- as.data.frame(table(ware$worried)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage)) |> 
  mutate(Ordered = fct_relevel(Var1, c('Nearly always', 
                                       'Most of the time',
                                       'Some of the time',
                                       'Rarely'))) |> 
  arrange(Ordered) |> 
  gt() |> 
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
           felt worried since January 2022.")

