library(qualtRics)
library(Hmisc)
library(tidyverse)
library(gt)
library(gtExtras)

data <- read_survey("qualtrics-data.csv")
data <- data |> 
  filter(!is.na(county)) |> 
  mutate(county = tolower(county))

##### Function #####

vartable <- function(variable, labels, question){
  df <- as.data.frame(table(variable)) |> # create data frame
    mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
    arrange(desc(Percentage)) |> 
    mutate(Ordered = fct_relevel(variable, 
                                 labels))  |> 
    arrange(Ordered) |> 
    gt() |> 
    cols_label(
      variable = "Response",
      Freq = "Count") |>
    cols_align(
      align = "left",
      columns = variable
    ) |> 
    tab_options(table.align = "left") |> 
    cols_hide(Ordered) |> 
    tab_header(question)
}

##### All Counties #####

household_income <- vartable(data$household_income,
                             c('Greater than $90,000',
                               '$70,001 - $90,000',
                               '$50,001 - $70,000',
                               '$30,001 - $50,000',
                               '$0 - $30,000',
                               'Prefer not to answer'),
                             'What was your household income for 2022?')

single_parent <- vartable(data$single_parent,
                          c('Yes',
                            'No'),
                          "Do you live in a single parent household?")



housing <- vartable(data$housing,
                    c('I own my own home or apartment',
                      'I rent a house or apartment',
                      'Homeless, sheltered (i.e., shelter, or hotel/motel)',
                      'Homeless, unsheltered',
                      'Other (please specify)'),
                    "Which option best describes your current housing situation?")


##### Individual County Data #####

###Function to Create County Data ####

countydata <- function(countyname){
  df <- data |> 
    filter(county == countyname)
}

##### Lanier County #####

countydata <- function(countyname){
  df <- data |> 
    filter(county == "lanier")
}

##### Lanier ######

lanier <- countydata("lanier")

household_income_lanier <- vartable(lanier$household_income,
                                    c('Greater than $90,000',
                                      '$70,001 - $90,000',
                                      '$50,001 - $70,000',
                                      '$30,001 - $50,000',
                                      '$0 - $30,000',
                                      'Prefer not to answer'),
                                    'What was your household income for 2022?')
