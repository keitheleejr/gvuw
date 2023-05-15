library(qualtRics)
library(maps)
library(mapproj)
library(Hmisc)
library(tidyverse)
library(gt)
library(gtExtras)

data <- read_survey("qualtrics-data.csv")
data <- data |> 
  filter(!is.na(county))

##### MAPS #####

###### All Counties ######

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



all_county_map <- ggplot(map_data, aes(x = long, 
                     y = lat, 
                     fill = n,
                     group = n)) +
  geom_polygon(color = "black") + 
  coord_map() +
  theme_void() +
  scale_fill_gradient(name = "Number of Responses",
                      low="#fafcff", 
                      high="#1757bf", 
                      na.value = "grey75")

all_county_map


###### Atkinson #####

map_data <- data |> 
  mutate(county = tolower(county)) |> 
  drop_na(county) |> 
  group_by(county) |> 
  count()

atkinson_geo <- map_data("county") |> 
  rename(county = subregion) |>  
  filter(region == "georgia",
         county == "atkinson") 



atkinson_map_data <- inner_join(atkinson_geo, map_data,
                               by = "county")



atkinson_map <- ggplot(atkinson_map_data, aes(x = long, 
                                            y = lat, 
                                            fill = n,
                                            group = n)) +
  geom_polygon(color = "black",
               show.legend = FALSE) + 
  coord_map() +
  theme_void() +
  scale_fill_gradient(name = "Number of Responses",
                      low="#6b9be3", 
                      high="#6b9be3", 
                      na.value = "grey75")
atkinson_map


###### Bacon #######

map_data <- data |> 
  mutate(county = tolower(county)) |> 
  drop_na(county) |> 
  group_by(county) |> 
  count()

bacon_geo <- map_data("county") |> 
  rename(county = subregion) |>  
  filter(region == "georgia",
         county == "bacon") 



bacon_map_data <- inner_join(bacon_geo, map_data,
                                by = "county")



bacon_map <- ggplot(bacon_map_data, aes(x = long, 
                                              y = lat, 
                                              fill = n,
                                              group = n)) +
  geom_polygon(color = "black",
               show.legend = FALSE) + 
  coord_map() +
  theme_void() +
  scale_fill_gradient(name = "Number of Responses",
                      low="#6b9be3", 
                      high="#6b9be3", 
                      na.value = "grey75")
bacon_map

###### Berrien ######

map_data <- data |> 
  mutate(county = tolower(county)) |> 
  drop_na(county) |> 
  group_by(county) |> 
  count()

berrien_geo <- map_data("county") |> 
  rename(county = subregion) |>  
  filter(region == "georgia",
         county == "berrien") 



berrien_map_data <- inner_join(berrien_geo, map_data,
                                by = "county")



berrien_map <- ggplot(berrien_map_data, aes(x = long, 
                                              y = lat, 
                                              fill = n,
                                              group = n)) +
  geom_polygon(color = "black",
               show.legend = FALSE) + 
  coord_map() +
  theme_void() +
  scale_fill_gradient(name = "Number of Responses",
                      low="#6b9be3", 
                      high="#6b9be3", 
                      na.value = "grey75")
berrien_map

###### Brooks ######

map_data <- data |> 
  mutate(county = tolower(county)) |> 
  drop_na(county) |> 
  group_by(county) |> 
  count()

brooks_geo <- map_data("county") |> 
  rename(county = subregion) |>  
  filter(region == "georgia",
         county == "brooks") 



brooks_map_data <- inner_join(brooks_geo, map_data,
                                by = "county")



brooks_map <- ggplot(brooks_map_data, aes(x = long, 
                                              y = lat, 
                                              fill = n,
                                              group = n)) +
  geom_polygon(color = "black",
               show.legend = FALSE) + 
  coord_map() +
  theme_void() +
  scale_fill_gradient(name = "Number of Responses",
                      low="#6b9be3", 
                      high="#6b9be3", 
                      na.value = "grey75")
brooks_map

###### Clinch #####

map_data <- data |> 
  mutate(county = tolower(county)) |> 
  drop_na(county) |> 
  group_by(county) |> 
  count()

clinch_geo <- map_data("county") |> 
  rename(county = subregion) |>  
  filter(region == "georgia",
         county == "clinch") 



clinch_map_data <- inner_join(clinch_geo, map_data,
                              by = "county")



clinch_map <- ggplot(clinch_map_data, aes(x = long, 
                                          y = lat, 
                                          fill = n,
                                          group = n)) +
  geom_polygon(color = "black",
               show.legend = FALSE) + 
  coord_map() +
  theme_void() +
  scale_fill_gradient(name = "Number of Responses",
                      low="#6b9be3", 
                      high="#6b9be3", 
                      na.value = "grey75")
clinch_map

###### Coffee ######

map_data <- data |> 
  mutate(county = tolower(county)) |> 
  drop_na(county) |> 
  group_by(county) |> 
  count()

coffee_geo <- map_data("county") |> 
  rename(county = subregion) |>  
  filter(region == "georgia",
         county == "coffee") 



coffee_map_data <- inner_join(coffee_geo, map_data,
                              by = "county")



coffee_map <- ggplot(coffee_map_data, aes(x = long, 
                                          y = lat, 
                                          fill = n,
                                          group = n)) +
  geom_polygon(color = "black",
               show.legend = FALSE) + 
  coord_map() +
  theme_void() +
  scale_fill_gradient(name = "Number of Responses",
                      low="#6b9be3", 
                      high="#6b9be3", 
                      na.value = "grey75")
coffee_map

###### Echols ######

map_data <- data |> 
  mutate(county = tolower(county)) |> 
  drop_na(county) |> 
  group_by(county) |> 
  count()

echols_geo <- map_data("county") |> 
  rename(county = subregion) |>  
  filter(region == "georgia",
         county == "echols") 



echols_map_data <- inner_join(echols_geo, map_data,
                              by = "county")



echols_map <- ggplot(echols_map_data, aes(x = long, 
                                          y = lat, 
                                          fill = n,
                                          group = n)) +
  geom_polygon(color = "black",
               show.legend = FALSE) + 
  coord_map() +
  theme_void() +
  scale_fill_gradient(name = "Number of Responses",
                      low="#6b9be3", 
                      high="#6b9be3", 
                      na.value = "grey75")
echols_map

###### Jeff Davis ########

map_data <- data |> 
  mutate(county = tolower(county)) |> 
  drop_na(county) |> 
  group_by(county) |> 
  count()

jeffdavis_geo <- map_data("county") |> 
  rename(county = subregion) |>  
  filter(region == "georgia",
         county == "jeff davis") 



jeffdavis_map_data <- inner_join(jeffdavis_geo, map_data,
                                 by = "county")



jeffdavis_map <- ggplot(jeffdavis_map_data, aes(x = long, 
                                                y = lat, 
                                                fill = n,
                                                group = n)) +
  geom_polygon(color = "black",
               show.legend = FALSE) + 
  coord_map() +
  theme_void() +
  scale_fill_gradient(name = "Number of Responses",
                      low="#6b9be3", 
                      high="#6b9be3", 
                      na.value = "grey75")
jeffdavis_map

###### Lanier ######

map_data <- data |> 
  mutate(county = tolower(county)) |> 
  drop_na(county) |> 
  group_by(county) |> 
  count()

lanier_geo <- map_data("county") |> 
  rename(county = subregion) |>  
  filter(region == "georgia",
         county == "lanier") 



lanier_map_data <- inner_join(lanier_geo, map_data,
                              by = "county")



lanier_map <- ggplot(lanier_map_data, aes(x = long, 
                                          y = lat, 
                                          fill = n,
                                          group = n)) +
  geom_polygon(color = "black",
               show.legend = FALSE) + 
  coord_map() +
  theme_void() +
  scale_fill_gradient(name = "Number of Responses",
                      low="#6b9be3", 
                      high="#6b9be3", 
                      na.value = "grey75")
lanier_map

###### Lowndes #####



map_data <- data |> 
  mutate(county = tolower(county)) |> 
  drop_na(county) |> 
  group_by(county) |> 
  count()

lowndes_geo <- map_data("county") |> 
  rename(county = subregion) |>  
  filter(region == "georgia",
         county == "lowndes") 



lowndes_map_data <- inner_join(lowndes_geo, map_data,
                       by = "county")



lowndes_map <- ggplot(lowndes_map_data, aes(x = long, 
                                       y = lat, 
                                       fill = n,
                                       group = n)) +
  geom_polygon(color = "black",
               show.legend = FALSE) + 
  coord_map() +
  theme_void() +
  scale_fill_gradient(name = "Number of Responses",
                      low="#6b9be3", 
                      high="#6b9be3", 
                      na.value = "grey75")
lowndes_map



###### Ware #######

map_data <- data |> 
  mutate(county = tolower(county)) |> 
  drop_na(county) |> 
  group_by(county) |> 
  count()

ware_geo <- map_data("county") |> 
  rename(county = subregion) |>  
  filter(region == "georgia",
         county == "ware") 



ware_map_data <- inner_join(ware_geo, map_data,
                            by = "county")



ware_map <- ggplot(ware_map_data, aes(x = long, 
                                      y = lat, 
                                      fill = n,
                                      group = n)) +
  geom_polygon(color = "black",
               show.legend = FALSE) + 
  coord_map() +
  theme_void() +
  scale_fill_gradient(name = "Number of Responses",
                      low="#6b9be3", 
                      high="#6b9be3", 
                      na.value = "grey75")
ware_map

#### ALL COUNTIES ####

###### household_income ####

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
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("What was your household income for 2022?")

hi_table

###### single_parent #####

single_parent <- as.data.frame(table(data$single_parent)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage))


sp_table <- single_parent |> 
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

sp_table



###### housing #####

housing <- as.data.frame(table(data$housing)) |> # create data frame
  filter(Var1 != "Other (please specify)") |> 
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage))


housing_table <- housing |> 
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

housing_table

###### food #####
 


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
  tab_options(table.align = "left") |> 
  cols_hide(Ordered) |> 
  tab_header("How often, if at all, is there enough food in 
             your household to feed all household members?")

  

food_table





###### internet_access #####

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
  cols_hide(Ordered) |> 
  tab_header("At home, how reliable 
             is your internet access?" )
ia_table

###### internet_devices #####



###### child_care #####

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
  cols_hide(Ordered) |> 
  tab_header("Do you require childcare?")

cc_table

###### child_care_needs #####

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
  cols_hide(Ordered) |> 
  tab_header("Which option best 
             describes your child care needs?")

ccn_table


###### services #####

services_plot <- as_tibble(unlist(strsplit(data$services,",")))

sn_plot <- services_plot |> 
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

sn_plot

###### organizations #####

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

###### resources_non_access #####


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

###### why_non_access  #####


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



###### Happy #####

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
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt happy since January 2022.")

happy_table


###### sad #####

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
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt sad since January 2022.")

sad_table


###### stressed #####

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
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt stressed since January 2022.")

stressed_table

###### angry #####

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
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt angry since January 2022.")

angry_table

###### frustrated #####

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
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt frustrated since January 2022.")

frustrated_table

###### tired #####

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
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt tired since January 2022.")

tired_table

###### hopeful #####

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
  cols_hide(Ordered)|> 
  tab_header("Please rate how often you have 
             felt hopeful since January 2022.")

hopeful_table

###### worried #####

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
  cols_hide(Ordered) |> 
  tab_header("Please rate how often you have 
             felt worried since January 2022.")

worried_table




##### Atkinson #####

atkinson <- data |> 
  filter(county == "Atkinson")

household_income_atkinson <- as.data.frame(table(atkinson$household_income)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage))



hi_table_atkinson <- household_income_atkinson |> 
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
  tab_header("What was your household income for 2022?")

hi_table_atkinson



single_parent_atkinson <- as.data.frame(table(atkinson$single_parent)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage))


sp_table_atkinson <- single_parent_atkinson |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Do you live in a single parent household?") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns

sp_table_atkinson



food_atkinson <- as.data.frame(table(atkinson$food)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage))


food_table_atkinson <- food_atkinson |> 
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
  cols_hide(Ordered) |> 
  tab_header("How often, if at all, is there enough food in 
             your household to feed all household members?")

food_table_atkinson




### Lowndes ######################

lowndes <- data |> 
  filter(county == "Lowndes")

household_income_lowndes <- as.data.frame(table(lowndes$household_income)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage))



hi_table_lowndes <- household_income_lowndes |> 
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
  tab_header("What was your household income for 2022?")

hi_table_lowndes



single_parent_lowndes <- as.data.frame(table(lowndes$single_parent)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage))


sp_table_lowndes <- single_parent_lowndes |> 
  gt() |>
  cols_label(
    Var1 = "Response",
    Freq = "Count") |>
  tab_header("Do you live in a single parent household?") |> 
  cols_align(
    align = "left",
    columns = Var1
  ) # add additional cols_align if need to align additional columns

sp_table_lowndes



food_lowndes <- as.data.frame(table(lowndes$food)) |> # create data frame
  mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
  arrange(desc(Percentage))


food_table_lowndes <- food_lowndes |> 
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
  cols_hide(Ordered) |> 
  tab_header("How often, if at all, is there enough food in 
             your household to feed all household members?")

food_table_lowndes

