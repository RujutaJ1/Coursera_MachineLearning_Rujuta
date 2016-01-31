# Coursera Project For Machine Learning 

traindataurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(traindataurl, destfile="./TrainData.csv")
TrainData <- read.csv("./TrainData.csv", header =TRUE)
dim(TrainData)
# understand a little bit about the Train Data
#names(TrainData)
# for cross validation, i need to make a part of my data, for training and testing, and then use
# the test data only for running the final model. 

#testdataurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
#download.file(testdataurl, destfile="./TestData.csv")
TestData <- read.csv("./TestData.csv", header=TRUE)

dim(TestData)

# Exploratory Analysis 

str(TrainData)
# understand the missing values 
sum(is.na(trainData))
na_count2 <- sapply(TrainData, function(y) sum(length(which(is.na(y)))))
View(na_count2)

# Steps in Data Preparation 
# 1. making the partition in Training Data - training and validation data sets
Trainpart <- createDataPartition (TrainData$classe, p=0.7, list=FALSE)
TrainingSubset <- TrainData[Trainpart,]
TestSubset <- TrainData[-Trainpart,]
dim(TrainingSubset)
dim(TestSubset)

# 2. Clean the data by  a] Clearing near zero variance variables, b] Clearing empty columns
nz <- nearZeroVar(TrainingSubset, saveMetrics=TRUE)
dim(nz)
myTraining <- TrainingSubset[,nz$nzv==FALSE]
dim(myTraining)

nz2 <- nearZeroVar(TestSubset, saveMetrics=TRUE)
dim(nz2)
myTest <- TestSubset[, nz2$nzv==FALSE]
dim(myTest)

#2b. Removing the first column 
myTraining <- myTraining [c(-1)]
#2c  Remove the columns that have mostly NA- Removing for 60%

trainingroughwork <- myTraining

for (i in 1: length(myTraining))
{if (sum(is.na(myTraining[,i]))/nrow(myTraining) >= 0.6)
{for(j in 1: length(trainingroughwork))
{if (length(grep(names(myTraining[i]),
                 names(trainingroughwork)[j]))==1)
{trainingroughwork <- trainingroughwork[,-j]}
}

}
}

myTraining <- trainingroughwork

#2d now do this procedure for the myTest data(validation set ) and the testing data
smallsetcolumnnames <- colnames(myTraining)
smallsetcolname2 <- colnames(myTraining[,-58])

myTest <- myTest[smallsetcolumnnames]
TestData <- TestData[smallsetcolname2]

# 2e - make sure that the data in Training set, validation set and the test set is in the same format




for (i in 1:length(TestData) ) {
  for(j in 1:length(myTraining)) {
    if( length( grep(names(myTraining[i]), names(TestData)[j]) ) == 1)  {
      class(TestData[j]) <- class(myTraining[i])
    }      
  }      
}

# To get the same class between TestData and myTraining
TestData <- rbind(myTraining[2, -58] , TestData)
TestData <- TestData[-1,]

# Step 3 : Prediction using Decision Trees and Random Forest

# Prediction using Decision Tree Analysis

set.seed(11111)
library(rpart)
library(rpart.plot)
library(rattle)
ModelFitA1 <- rpart(classe~.,data=myTraining,method="class")

predictionsA1 <- predict(ModelFitA1, myTest, type = "class")
cmtree <- confusionMatrix(predictionsA1, myTest$classe)
cmtree

plot(cmtree$table, col = cmtree$byClass, main = 
       paste("Decision Tree Confusion Matrix: Accuracy =", 
             round(cmtree$overall['Accuracy'], 4)))
# Prediction with Random Forests

set.seed(22222)
library(randomForest)
ModelFitB1 <- randomForest(classe~., data=myTraining)
predictionB1 <- predict(ModelFitB1, myTest, type="class")
cmrf <- confusionMatrix(predictionB1, myTest$classe)
cmrf
plot(ModelFitB1)
plot(cmrf$table, col=cmtree$byClass, 
     main=paste("Random Forest Confusion Matrix: 
                Accuracy =", round(cmrf$overall['Accuracy'],4)))


# Method 3 : Prediction with Generalised Boosted Regression 

set.seed(33333)
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 1)

gbmFit1 <- train(classe ~ ., data=myTraining, method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE)


gbmFinMod1 <- gbmFit1$finalModel

gbmPredTest <- predict(gbmFit1, newdata=myTest)
gbmAccuracyTest <- confusionMatrix(gbmPredTest, myTest$classe)
gbmAccuracyTest

plot(gbmFit1, ylim=c(0.9,1))

#Step 4 : Now that we know that we have high accuracy on the validation data set, lets predict for the test data

PredictedResults <- predict(ModelFitB1, TestData, type="class")
PredictedResults
