# tidyverse includes readxl package
library(tidyverse)
library(readxl)

# Set working directory to lab_2-swoshard/src/data
setwd("~/lab_2-swoshard/src/data") # This doesn't work for all machines :(


# Read in main given data for Lab 2
# Specify the sheet, read starting at row 2
X <- read_excel("../../data/raw/covid-19.xlsx", 
                sheet = "Covid-19",
                range = cell_rows(c(2, NA)))

Y <- read_csv("../../data/raw/mobility_data.csv")

# Write as .csv into interim data folder for processing
write_csv(X, path = "../../data/interim/state_data.csv")
write_csv(Y, path = "../../data/interim/full_data.csv")


