#### Input data set plants scale into Data Frame "plantsData"
crimeData <- read.csv(file = "C:/Users/maryjoyce/git/COSC757/GroupProject/UCI/communities.data",
                      header = FALSE, sep = ",", stringsAsFactors = TRUE,
                      col.names = c("state_numeric", "county_numeric", "community_numeric", "community_name_string", "fold_numeric",
                                    "population_numeric", "household_size_numeric", "race_percent_black_numeric", "race_percent_white_numeric",
                                    "race_percent_asian_numeric", "race_percent_hispanic_numeric", "age_percent_12_to_21_numeric",
                                    "age_percent_12_to_29_numeric", "age_percent_16_to_24_numeric", "age_percent_65_up_numeric",
                                    "number_urban_numeric", "percent_urban_numeric", "median_income_numeric", "percent_with_wage_numeric",
                                    "percent_with_farm_self_numeric", "percent_with_investment_income_numeric", "percent_with_social_security_numeric",
                                    "pctWPubAsst_numeric", "pctWRetire_numeric", "medFamInc_numeric", "perCapInc_numeric", "whitePerCap_numeric",
                                    "blackPerCap_numeric", "indianPerCap_numeric", "AsianPerCap_numeric", "OtherPerCap_numeric",
                                    "HispPerCap_numeric", "NumUnderPov_numeric", "PctPopUnderPov_numeric", "PctLess9thGrade_numeric",
                                    "PctNotHSGrad_numeric", "PctBSorMore_numeric", "PctUnemployed_numeric", "PctEmploy_numeric",
                                    "PctEmplManu_numeric", "PctEmplProfServ_numeric", "PctOccupManu_numeric", "PctOccupMgmtProf_numeric",
                                    "MalePctDivorce_numeric", "MalePctNevMarr_numeric", "FemalePctDiv_numeric", "TotalPctDiv_numeric",
                                    "PersPerFam_numeric","PctFam2Par_numeric", "PctKids2Par_numeric","PctYoungKids2Par_numeric",
                                    "PctTeen2Par_numeric", "PctWorkMomYoungKids_numeric", "PctWorkMom_numeric", "NumIlleg_numeric",
                                    "PctIlleg_numeric", "NumImmig_numeric", "PctImmigRecent_numeric", "PctImmigRec5_numeric",
                                    "PctImmigRec8_numeric", "PctImmigRec10_numeric", "PctRecentImmig_numeric", "PctRecImmig5_numeric",
                                    "PctRecImmig8_numeric", "PctRecImmig10_numeric", "PctSpeakEnglOnly_numeric", "PctNotSpeakEnglWell_numeric",
                                    "PctLargHouseFam_numeric","PctLargHouseOccup_numeric", "PersPerOccupHous_numeric", "PersPerOwnOccHous_numeric",
                                    "PersPerRentOccHous_numeric", "PctPersOwnOccup_numeric", "PctPersDenseHous_numeric", "PctHousLess3BR_numeric",
                                    "MedNumBR_numeric", "HousVacant_numeric", "PctHousOccup_numeric", "PctHousOwnOcc_numeric",
                                    "PctVacantBoarded_numeric", "PctVacMore6Mos_numeric", "MedYrHousBuilt_numeric", "PctHousNoPhone_numeric",
                                    "PctWOFullPlumb_numeric", "OwnOccLowQuart_numeric", "OwnOccMedVal_numeric", "OwnOccHiQuart_numeric",
                                    "RentLowQ_numeric", "RentMedian_numeric", "RentHighQ_numeric", "MedRent_numeric", "MedRentPctHousInc_numeric",
                                    "MedOwnCostPctInc_numeric", "MedOwnCostPctIncNoMtg_numeric", "NumInShelters_numeric", "NumStreet_numeric",
                                    "PctForeignBorn_numeric","PctBornSameState_numeric", "PctSameHouse85_numeric", "PctSameCity85_numeric",
                                    "PctSameState85_numeric", "LemasSwornFT_numeric", "LemasSwFTPerPop_numeric", "LemasSwFTFieldOps_numeric",
                                    "LemasSwFTFieldPerPop_numeric", "LemasTotalReq_numeric", "LemasTotReqPerPop_numeric", "PolicReqPerOffic_numeric",
                                    "PolicPerPop_numeric", "RacialMatchCommPol_numeric", "PctPolicWhite_numeric", "PctPolicBlack_numeric",
                                    "PctPolicHisp_numeric", "PctPolicAsian_numeric", "PctPolicMinor_numeric", "OfficAssgnDrugUnits_numeric",
                                    "NumKindsDrugsSeiz_numeric", "PolicAveOTWorked_numeric", "LandArea_numeric", "PopDens_numeric",
                                    "PctUsePubTrans_numeric", "PolicCars_numeric", "PolicOperBudg_numeric", "LemasPctPolicOnPatr_numeric",
                                    "LemasGangUnitDeploy_numeric", "LemasPctOfficDrugUn_numeric", "PolicBudgPerPop_numeric", "ViolentCrimesPerPop_numeric"))

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
itemFrequencyPlot(votingBaskets, support=0.01, cex.names=0.8)


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
