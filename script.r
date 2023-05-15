library(qualtRics)
library(Hmisc)
library(tidyverse)
library(gt)
library(gtExtras)

data <- read_survey("qualtrics-data.csv")

# Map

map_data <- data |> 
  mutate(county = tolower(county)) |> 
  drop_na(county) |> 
  group_by(county) |> 
  count()

geo_counties <- map_data("county") |> 
  rename(county = subregion) |>  
  filter(region == "georgia",
         county %in% c("atkinson",
                       "bacon",
                       "berrien",
                       "brooks",
                       "clinch",
                       "coffee",
                       "echols",
                       "jeff davis",
                       "lanier",
                       "lowndes",
                       "ware")) 



map_data <- inner_join(geo_counties, map_data,
                       by = "county")



ggplot(map_data, aes(x = long, 
                     y = lat, 
                     fill = n,
                     group = n)) +
  geom_polygon(color = "black") + 
  coord_map() +
  theme_void() +
  scale_fill_gradient(low="white", high="red", na.value = "grey75") 











# str(data$zip_code)
# data$zip_code_factor <- as.factor(data$zip_code)
# levels(data$zip_code_factor)



# county

county_plot <- as_tibble(data$county)

county_plot |> 
  filter(!is.na(value)) |> 
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

# household_income

household_income <- as.data.frame(table(data$household_income)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage))



hi_table <- household_income |> 
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
  cols_hide(Ordered) |> 
  tab_header(
    title = "Household Income"
  )

hi_table

# single_parent

single_parent <- as.data.frame(table(data$single_parent)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage))


sp_table <- single_parent |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns

sp_table

# occupation

occupation_table <- as_tibble(unlist(strsplit(data$occupation,",")))

occupation_table |> 
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

# housing

housing <- as.data.frame(table(data$housing)) |> # create data frame
  filter(Var1 != "Other (please specify)") |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage))


housing_table <- housing |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns

housing_table

# food



food <- as.data.frame(table(data$food)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) 


food_table <- food |> 
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
  cols_hide(Ordered) 

  

food_table

# housing_stress

housing_stress <- as.data.frame(table(data$housing_stress)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) 

hs_table <- housing_stress |> 
  mutate(Ordered = fct_relevel(Var1, c("Very stressful",
                                       "Somewhat stressful",
                                       "Not stressful",
                                       "Not applicable, do not have to pay for this")
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
  cols_hide(Ordered) 
hs_table

  
# energy_stress

energy_stress <- as.data.frame(table(data$energy_stress)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) 

es_table <- energy_stress |> 
  mutate(Ordered = fct_relevel(Var1, c("Very stressful",
                                       "Somewhat stressful",
                                       "Not stressful",
                                       "Not applicable, do not have to pay for this")
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
  cols_hide(Ordered) 
es_table

# food_stress

food_stress <- as.data.frame(table(data$food_stress)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) 

fs_table <- food_stress |> 
  mutate(Ordered = fct_relevel(Var1, c("Very stressful",
                                       "Somewhat stressful",
                                       "Not stressful",
                                       "Not applicable, do not have to pay for this")
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
  cols_hide(Ordered) 
fs_table

# trans_stress

trans_stress <- as.data.frame(table(data$trans_stress)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) 

ts_table <- trans_stress |> 
  mutate(Ordered = fct_relevel(Var1, c("Very stressful",
                                       "Somewhat stressful",
                                       "Not stressful",
                                       "Not applicable, do not have to pay for this")
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
  cols_hide(Ordered) 
ts_table

# internet_access

internet_access <- as.data.frame(table(data$internet_access)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) 

ia_table <- internet_access |> 
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
  cols_hide(Ordered) 
ia_table

# internet_devices



# child_care

child_care <- as.data.frame(table(data$child_care)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) 

cc_table <- child_care |> 
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
  cols_hide(Ordered) 

cc_table

# child_care_needs

child_care_needs <- as.data.frame(table(data$child_care_needs)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) 

ccn_table <- child_care_needs |> 
  mutate(Ordered = fct_relevel(Var1, c("Full-time child care",
                                       "After-school child care",
                                       "Other (please specify)")
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
  cols_hide(Ordered) 

ccn_table

# child_care_stress

childcare_stress <- as.data.frame(table(data$child_care_stress)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) 

ccs_table <- childcare_stress |> 
  mutate(Ordered = fct_relevel(Var1, c("Very stressful",
                                       "Somewhat stressful",
                                       "Not stressful",
                                       "Not applicable, do not have to pay for this")
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
  cols_hide(Ordered) 

ccs_table

# clothing_stress

clothing_stress <- as.data.frame(table(data$clothing_stress)) |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100,2 )) 

cs_table <- clothing_stress |> 
  mutate(Ordered = fct_relevel(Var1, c("Very stressful",
                                       "Somewhat stressful",
                                       "Not stressful",
                                       "Not applicable, do not have to pay for this")
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
  cols_hide(Ordered) 

cs_table

# services

services_table <- as_tibble(unlist(strsplit(data$services,",")))

services_table |> 
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

# organizations

org_table <- as_tibble(unlist(strsplit(data$organizations,",")))

org_table |> 
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

# resources_non_access


rna_table <- as_tibble(unlist(strsplit(data$resources_non_access,",")))

rna_table |> 
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

# why_non_access


wna_table <- as_tibble(unlist(strsplit(data$why_non_access,",")))

wna_table |> 
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



# Happy

happy_table <- as.data.frame(table(data$happy)) |>  # create data frame 
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
  cols_hide(Ordered) 

happy_table


# sad

sad <- as.data.frame(table(data$sad)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
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
    Freq = "Count") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) 

sad_table


# stressed

stressed <- as.data.frame(table(data$stressed)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
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
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) 

stressed_table

# angry

angry <- as.data.frame(table(data$angry)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
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
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) 

angry_table

# frustrated

frustrated <- as.data.frame(table(data$frustrated)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
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
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) 

frustrated_table

# tired

tired <- as.data.frame(table(data$tired)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
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
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) 

tired_table

# hopeful

hopeful <- as.data.frame(table(data$hopeful)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
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
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered)

hopeful_table

# worried

worried <- as.data.frame(table(data$worried)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
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
    Freq = "Count") |>
  cols_align(
    align = "left",
    columns = Var1
  ) |> 
  cols_hide(Ordered) 

worried_table


