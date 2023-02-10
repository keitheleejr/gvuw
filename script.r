setwd("/Users/keithlee/Documents/gvuw")

library(qualtRics)
library(Hmisc)
library(tidyverse)
library(gt)
library(gtExtras)

data <- read_survey("qualtrics-data.csv")

View(data)

housing <- as.data.frame(table(data$housing)) %>% # create data frame
    mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) %>%
    arrange(desc(Percentage))

housing_table <- housing %>%
    gt() %>%
    cols_label(
        Var1 = "Response",
        Freq = "Count") %>%
    cols_align(
        align = "left",
        columns = Var1
        ) # add additional cols_align if need to align additional columns

housing_table


household_income <- as.data.frame(table(data$household_income)) %>%
    mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) %>%
    arrange(desc(Percentage))

household_income_table <- household_income %>%
    gt() %>%
    cols_label(
        Var1 = "Response",
        Freq = "Count") %>%
    cols_align(
        align = "left",
        columns = Var1
        ) # add additional cols_align if need to align additional columns

household_income_table


str(data$zip_code)
data$zip_code_factor <- as.factor(data$zip_code)
levels(data$zip_code_factor)

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
# sad
# stressed
# angry
# frustrated
# tired
# hopeful
# worried