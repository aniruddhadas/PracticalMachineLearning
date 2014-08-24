getwd()
setwd("C:/Users/and/Documents/GitHub/PracticalMachineLearning")
dir()
train <- read.csv2("pml-training.csv",sep = ",", na.strings = c("", "NA", "#DIV/0!") ) 
head(train)
validTrainFeatures <- colnames(train[colSums(is.na(train)) == 0])[-(1:7)]
validTrainData <- train[validTrainFeatures]
head(validTrainData)
complete <- function(x) {x[,sapply(x, function(y) !any(is.na(y)))] }
validTrainData <- complete(validTrainData)
for(i in c(2:ncol(validTrainData)-1)) {validTrainData[,i] = as.numeric(as.character(validTrainData[,i]))}
str(validTrainData)
library(caret)
inTrain <- createDataPartition(y=validTrainData$classe, p=0.7, list=FALSE)
validTrainDataTrain <- validTrainData[inTrain,]
validTrainDataTest <- validTrainData[-inTrain,]
library(randomForest)
random.forest <- train(validTrainDataTrain[,-53],
                       validTrainDataTrain$classe,
                       tuneGrid=data.frame(mtry=3),
                       trControl=trainControl(method="none")
)

varImp(random.forest)
#now do the same with the final testing dataset variables
testing <- read.csv2("pml-testing.csv",sep = ",", na.strings = c("", "NA", "#DIV/0!") ) 
validTestingFeatures <- colnames(testing[colSums(is.na(testing)) == 0])[-(1:7)][-53]
testingTuned <- testing[validTestingFeatures]
for(i in c(1:ncol(testingTuned))) {testingTuned[,i] = as.numeric(as.character(testingTuned[,i]))}
predictionsTest <- predict(random.forest, newdata=testingTuned)
predictionsTest
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
