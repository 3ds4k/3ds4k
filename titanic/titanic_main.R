# Titanic

# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(data.table)
library(caret)


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

# Vulnerable
trainData[, vulnerable := ifelse(sex == "female" | age < 16, T, F)]



# MODEL -------------------------------------------------------------------

targetVariable <- 'survived'
featuredVariables <- c('class',
                       'sex',
                       'age',
                       'fare')

totalSize <- nrow(trainData)
trainingRate <- 0.7

set.seed(805)
trainIndices <- sample(totalSize, round(totalSize * trainingRate))

training <- trainData[ trainIndices,]
testing <- trainData[-trainIndices,]

formula <- as.formula(paste0(targetVariable, " ~ ", paste(featuredVariables, collapse=' + ')))

model <- glm(formula, family=binomial(link='logit'), data=training)

testing[, prediction := (predict(model, testing, type="response") > 0.5 )]
testing <- testing[!is.na(prediction)]

totalTest <- nrow(testing)
accuracy <- sum(testing[, survived] == testing[, prediction])/totalTest

# RF
formula <- as.formula(paste0("factor(", targetVariable, ") ~ ", paste(featuredVariables, collapse=' + ')))
trainingRf <- training[, c(targetVariable, featuredVariables), with = F]
trainingRf <- na.omit(trainingRf)

model <- randomForest(formula, data=trainingRf)

testing[, prediction := predict(model, testing, type="response")]
testing <- testing[!is.na(prediction)]

totalTest <- nrow(testing)
accuracy <- sum(testing[, survived] == testing[, prediction])/totalTest

# XGBoost
formula <- as.formula(paste0("factor(", targetVariable, ") ~ ", paste(featuredVariables, collapse=' + ')))
training <- training[, c(targetVariable, featuredVariables), with = F]
training <- na.omit(trainingRf)

model <- train(formula, data = training, method = "xgbTree", metric = "Accuracy")

testing[, prediction := predict(model, testing)]
testing <- testing[!is.na(prediction)]

totalTest <- nrow(testing)
accuracy <- sum(testing[, survived] == testing[, prediction])/totalTest



# model <- train(formula, data = training, method = "logitBoost")
