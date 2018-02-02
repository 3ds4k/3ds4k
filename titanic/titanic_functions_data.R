# Functions to extract and clean data for the Titanic challenge

ReadTrainData <- function(file){
    trainData <- fread(file, sep = ",")
    newNames <- c("passangerId", "survived", "class", "name", "sex", "age", "nSameAgeRelatives",
                  "nDiffAgeRelatives", "ticket", "fare", "cabin", "harbour")
    setnames(trainData, names(trainData), newNames)
    
    trainData[, ":=" (survived = as.logical(survived),
                      class = as.factor(class),
                      sex = as.factor(sex),
                      harbour = as.factor(harbour))]
    return(trainData)    
}

SeparateHonorific <- function(name){
    string <- strsplit(name, ",")[[1]][2]
    string <- strsplit(string, "\\.")[[1]][1]
    honorific <- gsub(" ", "", string)
    honorific <- as.factor(honorific)
    return(honorific)
}

AssignMarried <- function(name, sex){
    if(is.na(name) | is.na(sex)){
        return(NA)
    }
    singleName <- grepl("\\(", name)
    if(singleName & sex == 'female'){
        return(TRUE)
    } else {
        return(NA)
    }
}
