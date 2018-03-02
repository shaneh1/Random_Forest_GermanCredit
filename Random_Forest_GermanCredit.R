#import libraries
library(rattle)
library(rpart.plot)
library(caret)
library(RColorBrewer)
library(randomForest)
data(GermanCredit)

#Build a function to examine dataset easily
examine_data = function(dataset) {
  dim(dataset)
  str(dataset)
  summary(dataset)
}
#Call function with GermanCredit Dataset.
examine_data(GermanCredit)

#Build function to split into train/test.
split_data = function(data, train, test) {
  dataPartition = sample(2, nrow(data), replace = TRUE, prob = c(train, test))
  trainData <<- data[dataPartition==1,]
  testData <<- data[dataPartition==2,]
}

#Call function
split_data(GermanCredit, 0.7,0.3)
trainData
testData

#Generate random forest regression trees.
fit = randomForest(Class ~., trainData, ntree = 1250)
summary(fit)

#Make predictions on test data
predicted = predict(fit, testData, type = 'class')

#Evaluate the performance
accur = confusionMatrix(testData$Class, predicted)
accur

