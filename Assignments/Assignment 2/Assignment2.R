#### Input data set iris into Data Frame "irisData"
irisData <- read.csv(file = "C:/Users/maryjoyce/Documents/Towson/Spring 2016/COSC 757/Assignments/Assignment2/UCI data/iris.data.txt", header = FALSE, sep = ",", stringsAsFactors = TRUE, col.names = c("sLength", "sWidth", "pLength", "pWidth", "class") )
irisData[1:20,]

## Decision Tree Classification
# install.packages("rpart")
# library(rpart)
set.seed(1234)
ind <- sample(2, nrow(irisData), replace=TRUE,
            prob=c(0.7,0.3))
trainData <- irisData[ind==1,]
testData <- irisData[ind==2,]
iris_rpart <- rpart(class ~ sLength + sWidth + pLength + pWidth, data = trainData)
printcp(iris_rpart)
plotcp(iris_rpart)
plot(iris_rpart)
text(iris_rpart, use.n=TRUE)
iris_pred <- predict(iris_rpart, newData = testData)
iris_pred

## Naive Bayes Classification
# install.packages("e1071")
# library(class)
# library(e1071)
classifier <- naiveBayes(class ~ sLength + sWidth + pLength + pWidth, data = trainData)
pred <- predict(classifier, testData[,-5])
table(pred,testData$class)

## Random Forest?
# install.packages("randomForest")
# library(randomForest)
fit <- randomForest(class ~ sLength + sWidth + pLength + pWidth, data = trainData)
print(fit)
importance(fit)
