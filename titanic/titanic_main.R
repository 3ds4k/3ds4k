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

# Married
trainData[, married := map2_lgl(name, sex, AssignMarried)]



