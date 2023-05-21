library(qualtRics)
library(Hmisc)
library(tidyverse)
library(gt)
library(gtExtras)

data <- read_survey("qualtrics-data.csv")
data <- data |> 
  filter(!is.na(county)) |> 
  mutate(county = tolower(county)) |> 
  mutate(county = recode(county, "jeff davis" = "jeffdavis"))

###### Subset County Data #####

counties <- c("all", "atkinson", "bacon", "berrien", 
              "brooks", "clinch", "coffee", "echols", 
              "jeffdavis", "lanier", "lowndes", "ware")

countydata <- function(countyname){
  df <- data |> 
    filter(county == countyname)
  assign(countyname,df,1)
}

for (county in 1:length(counties)){
  countydata(counties[county])
}

# Tables ####

# Function

vartable <- function(variable, levels, question, varname,county){
  df <- as.data.frame(table(variable)) |> # create data frame
    mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) |>
    arrange(desc(Percentage)) |> 
    mutate(Ordered = fct_relevel(variable, 
                                 levels))  |> 
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
  assign(paste(varname,county,sep = "_"),df,1)
}
# Household Income ####



# Levels 

hi_levels <- c("Greater than $90,000",
               "$70,001 - $90,000",
               "$50,001 - $70,000",
               "$30,001 - $50,000",
               "$0 - $30,000",
               "Prefer not to answer")

# Question

hi_question <- "What was your household income for 2022?"

# Tables

vartable(data$household_income, 
         hi_levels,
         hi_question,
         "household_income",
         "all")

vartable(atkinson$household_income,
         hi_levels, 
         hi_question,
         "household_income", 
         "atkinson")


vartable(bacon$household_income,
         hi_levels,
         hi_question,
         "household_income",
         "bacon")

vartable(berrien$household_income,
         hi_levels,
         hi_question,
         "household_income",
         "berrien")

vartable(brooks$household_income,
         hi_levels,
         hi_question,
         "household_income",
         "brooks")

vartable(clinch$household_income,
         hi_levels,
         hi_question,
         "household_income",
         "clinch")

vartable(coffee$household_income,
         hi_levels,
         hi_question,
         "household_income",
         "coffee")

vartable(echols$household_income,
         hi_levels,
         hi_question,
         "household_income",
         "echols")

vartable(jeffdavis$household_income,
         hi_levels,
         hi_question,
         "household_income",
         "jeffdavis")

vartable(lanier$household_income,
         hi_levels,
         hi_question,
         "household_income",
         "lanier")

vartable(lowndes$household_income,
         hi_levels,
         hi_question,
         "household_income",
         "lowndes")

vartable(ware$household_income,
         hi_levels,
         hi_question,
         "household_income",
         "ware")

# Single Parent #####

# Levels 

sp_levels <- c("Yes",
               "No")

# Question

sp_question <- "Do you live in a single parent household?" 

# Tables

vartable(data$single_parent, 
         sp_levels,
         sp_question,
         "single_parent",
         "all")

vartable(atkinson$single_parent,
         sp_levels, 
         sp_question,
         "single_parent", 
         "atkinson")


vartable(bacon$single_parent,
         sp_levels,
         sp_question,
         "single_parent",
         "bacon")

vartable(berrien$single_parent,
         sp_levels,
         sp_question,
         "single_parent",
         "berrien")

vartable(brooks$single_parent,
         sp_levels,
         sp_question,
         "single_parent",
         "brooks")

vartable(clinch$single_parent,
         sp_levels,
         sp_question,
         "single_parent",
         "clinch")

vartable(coffee$single_parent,
         sp_levels,
         sp_question,
         "single_parent",
         "coffee")

vartable(echols$single_parent,
         sp_levels,
         sp_question,
         "single_parent",
         "echols")

vartable(jeffdavis$single_parent,
         sp_levels,
         sp_question,
         "single_parent",
         "jeffdavis")

vartable(lanier$single_parent,
         sp_levels,
         sp_question,
         "single_parent",
         "lanier")

vartable(lowndes$single_parent,
         sp_levels,
         sp_question,
         "single_parent",
         "lowndes")

vartable(ware$single_parent,
         sp_levels,
         sp_question,
         "single_parent",
         "ware")


###### I am trying to make a nested loop ######

# for (i in 1:length(variables)){
#   for (j in 1:length(counties)){
#     vartable(variables[i], hi_levels,
#              hi_question, counties[j])
#   }
# }

