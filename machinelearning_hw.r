library(caret)
setwd("C:/Users/husa/Desktop/DataScientist/8_MachineLearning/assignment")

testing= read.csv("pml-testing.csv", header = TRUE, row.names = 1, fill = TRUE)
training = read.csv("pml-training.csv", header = TRUE, row.names = 1, fill = TRUE)

training2 = t(na.omit(t(training)))
training2 = training[, colSums(is.na(training) | training=="")==0]
# remove the first 10 columns which are not predictors that can be expanded to new data sets
training3 = training2[,-10:-1]
   
trainIndex = createDataPartition(training3$classe, p = 0.75, list=F)
training = training3[trainIndex,]
testing = training3[-trainIndex,]

modelFit = train(classe~., method = "rpart", data = training)
#print(modelFit$finalModel)
prediction=predict(modelFit, newdata = testing)
confusionMatrix(testing$classe, prediction)
#0.49 is the accuracy

modelFit2 = train(classe~., method = "gbm", data = training)
prediction=predict(modelFit2, newdata = testing)
confusionMatrix(testing$classe, prediction)
#0.49 is the accuracy

