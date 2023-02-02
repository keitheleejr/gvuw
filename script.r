setwd("/Users/keithlee/Documents/gvuw")

library(qualtRics)
library(Hmisc)
library(tidyverse)
library(gt)
library(gtExtras)

data <- read_survey("qualtrics-data.csv")


table <- function(var){
    var_df <- as.data.frame(table(data$var)) %>%
        mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) %>%
        arrange(desc(Percentage))
}

table <- function(var){
    var <- as.data.frame(table(data$var)) %>%
        mutate(Percentage = round(Freq / sum(Freq) * 100, 2)) %>%
        arrange(desc(Percentage))

    var_table <- var %>%
        gt() %>%
        cols_label(
            Var1 = "Response",
            Freq = "Count") %>%
        cols_align(
            align = "left",
            columns = Var1
        ) # add additional cols_align if need to align additional columns
        
    var_table
}

axw <- function(var){
    y <- var + 1
    print(y)
}

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
