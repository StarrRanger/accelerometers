#Sourcing Data
library(caret);library(magrittr)
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","pml-training.csv")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv","pml-testing.csv")
trainingDataSet<- read.csv("pml-training.csv", header=TRUE)
testingDataSet<- read.csv("pml-testing.csv", header=TRUE)

#Cleaning Data

#Remove variables with nearly zero variance
nzv <- nearZeroVar(trainingDataSet)
trainingDataSet <- trainingDataSet[, -nzv]
testingDataSet <- testingDataSet[, -nzv]

#Convert factor variables to numeric variables
asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)], 
                                                   asNumeric))
trainingDataSet %>%
      subset(select = -c(X, raw_timestamp_part_1, raw_timestamp_part_2, 
                         cvtd_timestamp, new_window, num_window, classe)) %>%
      factorsNumeric -> trainingDataSet[ , 6:159]

testingDataSet %>%
      subset(select = -c(X, raw_timestamp_part_1, raw_timestamp_part_2, 
                         cvtd_timestamp, new_window, num_window)) %>%
      factorsNumeric -> testingDataSet[ , 6:159]

#Remove variables that don't likely to be a predictor (X, user_name, etc)
trainingDataSet <- trainingDataSet[,-(1:5)]
testingDataSet <- testingDataSet[,-(1:5)]

#Remove columns that are almost always empty
mostlyNA <- sapply(trainingDataSet, function(x) mean(is.na(x))) > 0.95
trainingDataSet <- trainingDataSet[, mostlyNA==F]
testingDataSet <- testingDataSet[, mostlyNA==F]

#Split into training and validation set
set.seed(1116)
inTrain <- createDataPartition(y=trainingDataSet$classe, p=0.7, list=F)
training <- trainingDataSet[inTrain,]
validation <- trainingDataSet[-inTrain,]
dim(training) ; dim(validation)


#Training and Cross Validating Model

#Training a random forest
library(randomForest)
set.seed(1116)
modFitrf <- randomForest(classe~ ., data = training, importance=TRUE, ntree=100)
modFitrf

#Use model to predict classe in validation set
CrossVal <- predict(modFitrf, newdata=validation)

#Show confusion matrix to get estimate of out-of-sample error (cross validation)
confusionMatrix(validation$classe, CrossVal)



#Predicting on Testing Dataset
predFinal <- predict(modFitrf, newdata=testingDataSet)
predFinal