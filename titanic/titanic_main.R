# Titanic

# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(data.table)


# FUNCTIONS ---------------------------------------------------------------

source('titanic_functions_data.R')


# SETTINGS ----------------------------------------------------------------

dataPath <- "data/"
trainFile <- paste0(dataPath, 'train.csv')

# DATA CLEANING -----------------------------------------------------------

# Read data
trainData <- ReadTrainData(trainFile)

# Separate age in age and ageEstimated
trainData[, ageEstimated := (age > 1 & (age-floor(age) == 0.5))]
trainData[age > 1, age := floor(age)]

# Honorific
trainData[, honorific := sapply(name, SeparateHonorific)]

# Fare distribution
ggplot(trainData,aes(x=fare)) + 
    geom_histogram(data=subset(trainData, survived == TRUE),fill = "red", alpha = 0.2) +
    geom_histogram(data=subset(trainData, survived == FALSE),fill = "blue", alpha = 0.2)

trainData[, lowFare := F]
fareDistribution <- trainData[fare < 50, lowFare := T][, .(n = .N), by = c("survived", "lowFare")]
fareDistribution <- fareDistribution[, ":=" (p = round(n / sum(n) * 100, 2)), by = "lowFare"][order(lowFare, survived)]
fareDistribution <- dcast(fareDistribution, survived ~ lowFare, value.var = "p")


# Married
trainData[, married := map2_lgl(name, sex, AssignMarried)]
