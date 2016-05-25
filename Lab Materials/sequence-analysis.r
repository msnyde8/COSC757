# Load necessary libraries
library(Matrix)
library(arules)
library(arulesSequences)
#Import data from Zaki et al.
data(zaki)
as(zaki, "data.frame")

#Run spade algorithm with a support of 0.4
s1 <- cspade(x, parameter = list(support = 0.4), control = list(verbose = TRUE))

#Examine Results
summary(s1)
as(s1, "data.frame")

#For the next example, download the following text file:
#https://raw.githubusercontent.com/eberlitz/SPADE/master/delicious.sequence.txt
#This dataset includes sequences of bookmark tags from the website www.delicious.com

#Set workspace to the location where sequence data is located
setwd("<workspace>")

#Read the dataset as a transaction dataset
x <- read_baskets(con  = "delicious.sequence.txt",info = c("sequenceID","eventID","SIZE"))

#Run Spade algorithm with a very low support
s1 <- cspade(x, parameter = list(support = 0.001), control = list(verbose = TRUE))

#Examine the results
summary(s1)
as(s1,"data.frame")

#Run Spade with a higher support.  What happened?
s1 <- cspade(n, parameter = list(support = 0.1), control = list(verbose = TRUE))
summary(s1)
as(s1,"data.frame")