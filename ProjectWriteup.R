getwd()
setwd("C:/and/PracticalMachineLearning")
dir()
#read the data
data <- read.csv2("pml-training.csv",sep = ",", na.strings = c("", "NA", "#DIV/0!") ) 
data <- read.csv2("pml-training.csv",sep = ",") 
head(data)
finaltesting <- read.csv2("pml-testing.csv",sep = ",", na.strings = c("", "NA", "#DIV/0!") ) 
# ignore name, timestamp etc. columns
validFeatures <- colnames(data[colSums(is.na(data)) == 0])[-(1:7)]
validTrainingData <- data[validFeatures]
#validTrainingData$classe <- as.factor(validTrainingData$classe)
#we will not be touching the test data
inTrain <- createDataPartition(y=validTrainingData$classe, p=0.7, list=FALSE)
validTrainingData <- validTrainingData[inTrain,]
validTestingData <- validTrainingData[-inTrain,]

complete <- function(x) {x[,sapply(x, function(y) !any(is.na(y)))] }
validTrainingData <- complete(validTrainingData)
for(i in c(2:ncol(validTrainingData)-1)) {validTrainingData[,i] = as.numeric(as.character(validTrainingData[,i]))}

validTestingData <- complete(validTestingData)
for(i in c(2:ncol(validTestingData)-1)) {validTestingData[,i] = as.numeric(as.character(validTestingData[,i]))}



#modFit <- train(classe~.,method="rf",data=validTrainingData,importance=TRUE)
cData <- validTrainingData
for(i in c(2:ncol(cData)-1)) {cData[,i] = as.numeric(as.character(cData[,i]))}

library(randomForest)
random.forest <- train(cData[,-57],
                       cData$classe,
                       tuneGrid=data.frame(mtry=3),
                       trControl=trainControl(method="none")
                    )


predictionsTr <- predict(random.forest, newdata=cData)
confusionMatrix(predictionsTr,cData$classe)

predictionsTe <- predict(random.forest, newdata=validTestingData)
confusionMatrix(predictionsTe,validTestingData$classe)

predictionsTe <- predict(modFit, newdata=validTestingData)
confusionMatrix(predictionsTe,validTestingData$classe)

