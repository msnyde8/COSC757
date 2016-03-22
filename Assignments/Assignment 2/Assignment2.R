#### Input data set bakabce scale into Data Frame "balanceScaleData"
balanceScaleData <- read.csv(file = "C:/Users/maryjoyce/Documents/Towson/Spring 2016/COSC 757/Assignments/Assignment2/UCI data/balance-scale.data", header = FALSE, sep = ",", stringsAsFactors = TRUE, col.names = c("class", "lWeight", "lDistance", "rWeight", "rDistance"))
balanceScaleData[1:20,]

## Decision Tree Classification
# install.packages("rpart")
# library(rpart)
set.seed(1234)
ind <- sample(2, nrow(balanceScaleData), replace=TRUE,
            prob=c(0.7,0.3))
trainData <- balanceScaleData[ind==1,]
testData <- balanceScaleData[ind==2,]
# trainData$balanceData <- (trainData$rWeight * trainData$rDistance) - (trainData$lWeight * trainData$lDistance)
# testData$balanceData <- (testData$rWeight * testData$rDistance) - (testData$lWeight * testData$lDistance)
# balanceScale_rpart <- rpart(class ~ balanceData, data = trainData)
balanceScale_rpart <- rpart(class ~ rWeight + rDistance + lWeight + lDistance, data = trainData, method = "class")
printcp(balanceScale_rpart)
plotcp(balanceScale_rpart)
plot(balanceScale_rpart)
text(balanceScale_rpart, use.n=TRUE)
balanceScale_pred <- predict(balanceScale_rpart, newData = testData)
balanceScale_pred

## Naive Bayes Classification
# install.packages("e1071")
# library(class)
# library(e1071)
# classifier <- naiveBayes(class ~ balanceData, data = trainData)
classifier <- naiveBayes(class ~ rWeight + rDistance + lWeight + lDistance, data = trainData, method = "class")
pred <- predict(classifier, testData[,-5])
table(pred,testData$class)

## Random Forest?
# install.packages("randomForest")
# library(randomForest)
# fit <- randomForest(class ~ balanceData, data = trainData)
fit <- randomForest(class ~ rWeight + rDistance + lWeight + lDistance, data = trainData, method = "class")
print(fit)
importance(fit)
