## Classification Using Neural Networks
# Load the iris dataset:
library(rpart)
library(nnet)
data(iris)

# Setup training and testing datasets
set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE,
              prob=c(0.7,0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

nn1 <- nnet(Species~., trainData, size=0, skip=TRUE, linout=TRUE)
nn2 <- nnet(Species~., trainData, size=2, rang=0.1, decay=5e-4, maxit=200)

table(testData$Species,predict(nn1, testData, type="class"))
table(testData$Species,predict(nn2, testData, type="class"))

# Setup another training and testing datasets
set.seed(2468)
ind <- sample(2, nrow(iris), replace=TRUE,
              prob=c(0.7,0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

nn1 <- nnet(Species~., trainData, size=0, skip=TRUE, linout=TRUE)
nn2 <- nnet(Species~., trainData, size=2, rang=0.1, decay=5e-4, maxit=200)

table(testData$Species,predict(nn1, testData, type="class"))
table(testData$Species,predict(nn2, testData, type="class"))


# SVM
library(e1071)
library(MASS)
data(cats)

# create a support vector model
model <- svm(Sex~., data=cats)
# inspect the parameters
print(model)
summary(model)
# scatterplot
plot(model,cats)

# create a training and testing dataset
set.seed(1234)
ind <- sample(2, nrow(cats), replace=TRUE,
              prob=c(0.7,0.3))
trainData <- cats[ind==1,]
testData <- cats[ind==2,]

model <- svm(Sex~., data=trainData)
prediction <- predict(model, testData[,-1])

# create the confusion matrix
tab <- table(pred=prediction, true=testData[,1])
# compute the sensitivity, specificity, and precision
classAgreement(tab)

# visualize results
plot(model, trainData)
plot(model, testData)

# tune the data
tuned <- tune.svm(Sex~., data=trainData, gamma = 10^(-6:-1), cost=10^(1:3))
summary(tuned)
model <- svm(Sex~., data=trainData,  gamma = 0.01, cost=10)
prediction2 <- predict(model, testData[,-1])




## Bagging
# load adabag library
library(adabag)
# load the Vehicle dataset
data(Vehicle)
# set variable to length of Vehicle dataset
l <- length(Vehicle[,1])
# create a sample for 2/3 of Vehicle dataset
sub <- sample(1:l,2*l/3)
# use bagging to create a classification model
Vehicle.bagging <- bagging(Class~.,data=Vehicle[sub,],mfinal=15,control=rpart.control(maxdepth=5, minsplit=15))
# predict accuracy
Vehicle.bagging.pred <- predict.bagging(Vehicle.bagging,newdata=Vehicle[-sub,],newmfinal=10)
# view the resulting confusion table
Vehicle.bagging.pred$confusion
# view the resulting prediction error
Vehicle.bagging.pred$error
# experiment
