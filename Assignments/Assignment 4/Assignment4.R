#### Input data set plants scale into Data Frame "plantsData"
votingData <- read.csv(file = "C:/Users/maryjoyce/Documents/Towson/Spring 2016/COSC 757/Assignments/Assignment4/UCI data/house-votes-84.data.txt", header = FALSE, sep = ",", stringsAsFactors = FALSE)
votingData.ID<-seq.int(nrow(votingData))
votingData[1:20,]
votingData$V1 <- as.factor(votingData$V1)
votingData$V2[votingData$V2=="y"]<-"1y"
votingData$V2[votingData$V2=="n"]<-"1n"
votingData$V2[votingData$V2=="?"]<-"1?"
votingData$V2 <- as.factor(votingData$V2)
votingData$V3[votingData$V3=="y"]<-"2y"
votingData$V3[votingData$V3=="n"]<-"2n"
votingData$V3[votingData$V3=="?"]<-"2?"
votingData$V3 <- as.factor(votingData$V3)
votingData$V4[votingData$V4=="y"]<-"3y"
votingData$V4[votingData$V4=="n"]<-"3n"
votingData$V4[votingData$V4=="?"]<-"3?"
votingData$V4 <- as.factor(votingData$V4)
votingData$V5[votingData$V5=="y"]<-"4y"
votingData$V5[votingData$V5=="n"]<-"4n"
votingData$V5[votingData$V5=="?"]<-"4?"
votingData$V5 <- as.factor(votingData$V5)
votingData$V6[votingData$V6=="y"]<-"5y"
votingData$V6[votingData$V6=="n"]<-"5n"
votingData$V6[votingData$V6=="?"]<-"5?"
votingData$V6 <- as.factor(votingData$V6)
votingData$V7[votingData$V7=="y"]<-"6y"
votingData$V7[votingData$V7=="n"]<-"6n"
votingData$V7[votingData$V7=="?"]<-"6?"
votingData$V7 <- as.factor(votingData$V7)
votingData$V8[votingData$V8=="y"]<-"7y"
votingData$V8[votingData$V8=="n"]<-"7n"
votingData$V8[votingData$V8=="?"]<-"7?"
votingData$V8 <- as.factor(votingData$V8)
votingData$V9[votingData$V9=="y"]<-"8y"
votingData$V9[votingData$V9=="n"]<-"8n"
votingData$V9[votingData$V9=="?"]<-"8?"
votingData$V9 <- as.factor(votingData$V9)
votingData$V10[votingData$V10=="y"]<-"9y"
votingData$V10[votingData$V10=="n"]<-"9n"
votingData$V10[votingData$V10=="?"]<-"9?"
votingData$V10 <- as.factor(votingData$V10)
votingData$V11[votingData$V11=="y"]<-"10y"
votingData$V11[votingData$V11=="n"]<-"10n"
votingData$V11[votingData$V11=="?"]<-"10?"
votingData$V11 <- as.factor(votingData$V11)
votingData$V12[votingData$V12=="y"]<-"11y"
votingData$V12[votingData$V12=="n"]<-"11n"
votingData$V12[votingData$V12=="?"]<-"11?"
votingData$V12 <- as.factor(votingData$V12)
votingData$V13[votingData$V13=="y"]<-"12y"
votingData$V13[votingData$V13=="n"]<-"12n"
votingData$V13[votingData$V13=="?"]<-"12?"
votingData$V13 <- as.factor(votingData$V13)
votingData$V14[votingData$V14=="y"]<-"13y"
votingData$V14[votingData$V14=="n"]<-"13n"
votingData$V14[votingData$V14=="?"]<-"13?"
votingData$V14 <- as.factor(votingData$V14)
votingData$V15[votingData$V15=="y"]<-"14y"
votingData$V15[votingData$V15=="n"]<-"14n"
votingData$V15[votingData$V15=="?"]<-"14?"
votingData$V15 <- as.factor(votingData$V15)
votingData$V16[votingData$V16=="y"]<-"15y"
votingData$V16[votingData$V16=="n"]<-"15n"
votingData$V16[votingData$V16=="?"]<-"15?"
votingData$V16 <- as.factor(votingData$V16)
votingData$V17[votingData$V17=="y"]<-"16y"
votingData$V17[votingData$V17=="n"]<-"16n"
votingData$V17[votingData$V17=="?"]<-"16?"
votingData$V17 <- as.factor(votingData$V17)
votingData[1:20,]

library("arules", lib.loc="~/R/win-library/3.2")
library(datasets)
#votingBaskets <- read.transactions(file = "C:/Users/maryjoyce/Documents/Towson/Spring 2016/COSC 757/Assignments/Assignment4/UCI data/house-votes-84.data.txt", format="basket", sep = ",")
# Inspect the dataset
votingBaskets <- as(votingData,"transactions")
summary(votingBaskets)
votingBaskets[1:2,]
# plot the data
itemFrequencyPlot(votingBaskets, support=0.1, cex.names=0.8)


## Apriori Algorithm
# association rules
rules <- apriori(votingBaskets, parameter = list(support=0.01, confidence=0.6, minlen=2))
# subset of rules
rulesDemocrat <- subset(rules, subset=rhs%in%"V1=democrat")
rulesRepublican <- subset(rules, subset=rhs%in%"V1=republican")
inspect(sort(rulesDemocrat, by="confidence")[1:5])
inspect(sort(rulesRepublican, by="confidence")[1:5])


## Eclat Algorithm
itemsets <- eclat(votingBaskets, parameter = list(sup=0.01, minlen=3, maxlen=15))
fsets <- eclat(votingBaskets, parameter=list(sup=0.01, minlen=3))
fsets.top5 <- sort(fsets)[1:5]
inspect(fsets.top5)
fsets.top10 <- sort(fsets)[1:10]
inspect(fsets.top10)
rulesDem <- subset(itemsets, subset=items%in%"V1=democrat")
rulesRep <- subset(itemsets, subset=items%in%"V1=republican")
inspect(sort(rulesDem, by="support")[1:5])
inspect(sort(rulesRep, by="support")[1:5])

rules2 <- apriori(votingBaskets, parameter = list(support=0.01, confidence=0.6, minlen=3, maxlen=15))
rulesDemocrat2 <- subset(rules2, subset=rhs%in%"V1=democrat")
rulesRepublican2 <- subset(rules2, subset=rhs%in%"V1=republican")
inspect(sort(rulesDemocrat2, by="support")[1:5])
inspect(sort(rulesRepublican2, by="support")[1:5])
