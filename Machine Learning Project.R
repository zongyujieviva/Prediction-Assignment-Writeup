library(AppliedPredictiveModeling)
library(ElemStatLearn)
library(lubridate)
library(ggplot2)
library(caret)
library(gbm)

### Data import
training <- read.csv("C:/Users/yzong/Documents/Coursera/testdir/pml-training.csv",na.strings = c("NA", "#DIV/0!", ""))
testing <- read.csv("C:/Users/yzong/Documents/Coursera/testdir/pml-testing.csv",na.strings = c("NA", "#DIV/0!", ""))

### Preliminary check
dim(training)
dim(testing)
table(training$classe)
prop.table(table(training$user_name))

#### Data Cleaning
training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0] 

classe <- training$classe
trainRemove <- grepl("^X|timestamp|window", names(training))
training <- training[, !trainRemove]
training <- training[, sapply(training, is.numeric)]
training$classe<- classe
testRemove <- grepl("^X|timestamp|window", names(testing))
testing <- testing[, !testRemove]
testing <- testing[, sapply(testing, is.numeric)]

dim(training)
dim(testing)

### Create training and testing data within the big training file
set.seed(3433)
inTrain = createDataPartition(training$classe, p = 0.7)[[1]]
mytrain = training[inTrain,]
mytest = training[-inTrain,]

### Try two models by using method = "rf" and "rpart"
mytrain$classe <- as.factor(mytrain$classe)
mytest$classe <- as.factor(mytest$classe)
set.seed(12345)
control <- trainControl(method="cv", 5)
F1 <- train(classe ~ ., method = "rf", data = mytrain,trControl=control, importance=TRUE, ntree=100)
F2 <- train(classe ~ ., method = "rpart", data = mytrain)

pred1 <- predict(F1,mytest)
pred2 <- predict(F2,mytest)
confusionMatrix(pred1, mytest$classe)$overall[1]
confusionMatrix(pred2, mytest$classe)$overall[1]

### use model 1 to fit the test data to predict the results
result <- predict(F1, testing[, -length(names(testing))])
result
