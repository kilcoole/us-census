library(ggplot2)
library(randomForest)
library(gridExtra)
library(corrplot)
library(knitr)

rawTrainingData <- read.csv("/Users/kilcoole/Documents/data/us_census_full/census_income_learn.csv", header=FALSE)

cenusColnames <- c("age","workerClass","industryCode","occuptationCode","adjGrossIncome","education","wagePerHour",
                   "educationLastWeek","maritialStatus","majorIndustryCode","majorOccupationCode","race","hispanicOrigin",
                   "sex","inLabourUnion","unemploymentReason","ftePTEStatus","capitalGains","capitalLosees","stockDividends",
                   "fedIncomeTaxLiability","taxFilerStatus","previousResidenceRegion","previousResidenceState","householdFamilyStatus",
                   "householdSummuary","instanceWeight","migCodeChangeInMSA","migCodeChangeInReg","migCodeChangeWithinReg","inHouseYearAgo",
                   "prevResInSunbelt","noOfPeopleEmployed","familyMembersUnder18","totalPersonEarnings","fatherBirthCountry","motherBirthCountry",
                   "ownBirthCountry","citizenship","totalPersonIncome","isSelfEmpolyed","taxableIncomeAmount","fillIncQuestVeteranAdmin",
                   "veteransBenefit","weeksWorkedInYear","year","incomeBin")

cenusColnames <- cenusColnames[!(cenusColnames %in% c("adjGrossIncome","fedIncomeTaxLiability","totalPersonEarnings","totalPersonIncome","fillIncQuestVeteranAdmin"))]

trainingData <- unique(rawTrainingData)
colnames(trainingData) <- cenusColnames
str(trainingData)
metadata <- data.frame(colnames(trainingData),sapply(trainingData,class))
#manually adjust known categoriwes
colnames(metadata) <- c("columnName","datatype")
additionalCat <- c("industryCode", "occuptationCode", "isSelfEmpolyed", "veteransBenefit","year")

metadata[metadata$columnName %in% additionalCat,"datatype"] <- as.factor("factor")
naCount <- data.frame(colnames(trainingData),sapply(trainingData,function(x) 100*sum(is.na(x))/length(x)))

numericColumns <- as.character(metadata[metadata$datatype %in% c("numeric","integer"),"columnName"])
catColumns <- as.character(metadata[metadata$datatype=="factor","columnName"])

#summary
sapply(trainingData[,numericColumns],summary)
sapply(trainingData[,catColumns],table)

#clean up the character columns: remove leading/trailing whitespace
trainingData[,catColumns] <- sapply(trainingData[,catColumns],function(x) gsub("^\\s+|\\s+$", "", as.character(x)))
x <- dput(metadata)
#question mark count
containsQuestionMark <- sapply(trainingData,function(x) sum(sapply(x, function(y) y=="?"))/length(x))

trainingData[,catColumns] <-lapply(trainingData[,catColumns],factor)
trainingData$incomeBin <- as.factor(trainingData$incomeBin)
rfFit <- randomForest(incomeBin ~.,data=trainingData,sampsize=c(2400,1200),strata=trainingData$incomeBin)
varImpPlot(rfFit)

trainingData$rfPredict <- predict(rfFit,trainingData)
table(trainingData$rfPredict,trainingData$incomeBin)

#predictor column
table(trainingData$incomeBin)
trainingData$over50K <- as.integer(trainingData$incomeBin=="50000+.")
table(trainingData$over50K)

#eda
educationString <- "Children, 7th and 8th grade, 9th grade, 10th grade, High school graduate, 11th grade, 12th grade no diploma, 5th or 6th grade, Less than 1st grade, Bachelors degree(BA AB BS), 1st 2nd 3rd or 4th grade, Some college but no degree, Masters degree(MA MS MEng MEd MSW MBA), Associates degree-occup /vocational, Associates degree-academic program, Doctorate degree(PhD EdD), Prof school degree (MD DDS DVM LLB JD)"
educationOrdered <- c("Children","Less than 1st grade")
educationOrdered <- as.factor(sapply(educationOrdered,function(x) gsub("^\\s+|\\s+$", "", x)))
trainingData$educationOrdered <- factor(trainingData$education,educationOrdered)
table(trainingData$educationOrdered)
chisq.test(table(trainingData$education,trainingData$over50K))
ggplot(trainingData,aes(x=educationOrdered,fill=incomeBin)) + 
  geom_bar() + coord_flip()

trainingData$isThirdLevel <- as.integer(trainingData$education %in% c("Bachelors degree(BA AB BS)","Masters degree(MA MS MEng MEd MSW MBA)",
                                                                      "Doctorate degree(PhD EdD)","Prof school degree (MD DDS DVM LLB JD)"))

100*table(trainingData$isThirdLevel,trainingData$incomeBin)/apply(table(trainingData$isThirdLevel,trainingData$incomeBin),1,sum)
#
summary(trainingData$capitalGains)
ggplot(trainingData,aes(x=incomeBin,y=capitalGains)) + 
  geom_boxplot()
#hmm alot of outliers lets remove
ggplot(trainingData,aes(x=capitalGains)) + 
  geom_density()

ggplot(trainingData,aes(x=incomeBin,y=age)) + 
  geom_boxplot()

#wage per hour
summary(trainingData$wagePerHour)
sum(trainingData$wagePerHour>0)
wagePlus <- trainingData[trainingData$wagePerHour>0,]
ggplot(trainingData,aes(x=wagePerHour)) + 
  geom_density()
ggplot(trainingData,aes(x=incomeBin,y=wagePerHour)) + 
  geom_boxplot()

ggplot(wagePlus,aes(x=incomeBin,y=wagePerHour)) + 
  geom_boxplot()
ggplot(wagePlus,aes(x=wagePerHour,fill=incomeBin)) + 
  geom_bar(alpha=0.3,binwidth=500)
ggplot(wagePlus,aes(x=wagePerHour,fill=incomeBin)) + 
  geom_density(alpha=0.3)


#numeric variables

numericSummaries <- lapply( trainingData[,numericColumns] , function(x) rbind(min = min(x),
                                        firstQ=quantile(x, probs = seq(0, 1, 0.25))[2],
                                        median = median(x),
                                        mean = mean(x),
                                        thirdQ = quantile(x, probs = seq(0, 1, 0.25))[4],
                                        max = max(x),
                                        count = as.integer(length(x)),
                                        uniqueCount = as.integer(length(unique(x)))))
tmp <- names(numericSummaries)
numericSummaries <- sapply(numericSummaries,function(x) round(x, 2))
numericSummaries <- data.frame(numericSummaries)
colnames(numericSummaries) <- tmp
numericSummaries <- cbind(c("min","firstQ","median","mean","thirdQ","max","count","uniqueCount"),numericSummaries)
colnames(numericSummaries)[1] <- "metric"
#numericSummaries[7,c(-1)] <- round(numericSummaries[7,c(-1)],0)
#numericSummaries[8,c(-1)] <- round(numericSummaries[8,c(-1)],0)
#disregard industryCode, occupationCode, isSelfEmployed, year and veteransBenefit as numeric: they seem to be categorical
sapply(trainingData[,numericColumns],summary)
cor1 <- cor(trainingData[,numericColumns[!(numericColumns %in% c("industryCode","occuptationCode","isSelfEmpolyed"))]])
corrplot(cor1,type="upper")

cor(trainingData$age,trainingData$veteransBenefit)
ggplot(trainingData,aes(x=age,y=veteransBenefit)) + 
  geom_point()
#hveterans benefit actually a category
#trainingData$veteransBenefit <- as.factor(trainingData$veteransBenefit)

#unemployed not in dataset - let's infer it
trainingData$isEmployed <- as.integer(trainingData$workerClass!="Not in universe" & trainingData$workerClass!="Never worked")
table(trainingData$isEmployed)
table(trainingData$isEmployed,trainingData$incomeBin)

#capital gains tax
ggplot(trainingData,aes(x=capitalGains,fill=incomeBin)) + 
  geom_density()
ggplot(trainingData,aes(x=incomeBin,y=capitalGains)) + 
  geom_boxplot()

#highly skewed
trainingData$logCG <- ifelse(trainingData$capitalGains<1,0,log(trainingData$capitalGains))
ggplot(trainingData,aes(x=logCG,fill=incomeBin)) + 
  geom_density(alpha=0.3)
ggplot(trainingData[trainingData$logCG>0,],aes(x=logCG,fill=incomeBin)) + 
  geom_density(alpha=0.3)

ggplot(trainingData[trainingData$logCG>0,],aes(x=sqrt(capitalGains),fill=incomeBin)) + 
  geom_density(alpha=0.3)
#capital gains tax may be a good predictor
trainingData$cgPlus <- as.integer(trainingData$capitalGains>0)
table(trainingData$cgPlus,trainingData$incomeBin)
# so a positive cgt could be a good predictor

#age
summary(trainingData$age)
ggplot(trainingData,aes(x=age)) + 
  geom_histogram(binwidth=1,col="red",fill="green",alpha=0.5)
# a good few are children - lets split those
trainingData$isChild <- as.integer(trainingData$age<18)
table(trainingData$isChild) # as expected children not earning
#look at the split for adults 
ggplot(trainingData[trainingData$isChild==0,],aes(x=incomeBin,y=age)) + 
  geom_boxplot()
ggplot(trainingData,aes(x=age,fill=incomeBin)) + 
  geom_density(alpha=0.3)
ggplot(trainingData,aes(x=age,fill=incomeBin)) + 
  geom_bar(alpha=0.3) + labs(fill = "Income bin")
#interquartile range - use this

#education
table(trainingData$education)
educationTable <- 100*table(trainingData$education,trainingData$incomeBin)/apply(table(trainingData$education,trainingData$incomeBin),1,sum)
#go back to age
ggplot(trainingData[trainingData$isChild==0,],aes(x=age,fill=factor(isThirdLevel))) + 
  geom_density(alpha=0.3) + facet_grid(.~incomeBin)
#even with a degree age is still a big factor in determining if someone earns over 50k

#industry
industryTable <- 100*table(trainingData$majorIndustryCode,trainingData$incomeBin)/apply(table(trainingData$majorIndustryCode,trainingData$incomeBin),1,sum)
ggplot(trainingData,aes(x=majorIndustryCode,y=age)) + 
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))

simpleLR <- glm(incomeBin~.,data=trainingData,family = "binomial")
#stock
ggplot(trainingData,aes(y=stockDividends,x=incomeBin)) + 
  geom_boxplot()

ggplot(trainingData[trainingData$stockDividends>0,],aes(x=log(stockDividends),fill=incomeBin)) + 
  geom_density(alpha=0.3) + xlab("Income bin") + ylab("Log Stock dividends (>0)")


trainingData$hasStockDividends <- as.integer(trainingData$stockDividends>0)
table(trainingData$hasStockDividends)
table(trainingData$hasStockDividends,trainingData$incomeBin)
chisq.test(table(trainingData$hasStockDividends,trainingData$incomeBin))

trainingData$rfPredict <- predict(rfFit,trainingData)
table(trainingData$rfPredict,trainingData$incomeBin)
rfSimple <- randomForest(incomeBin~isThirdLevel + isChild + hasStockDividends + cgPlus + isEmployed,data=trainingData)
rfSimplePredict <- predict(rfSimple,trainingData)


library(dplyr)
ce <-  ddply(trainingData, "incomeBin", mutate, binPct = Weight/sum(Weight) * 100)

incomeEdu <- select(trainingData,education,incomeBin) %>% 
  group_by(education)

ggplot(educationTable, aes(x=Var1, y=Freq, fill=Var2)) + 
  geom_bar(stat='identity') + 
  theme(axis.text.x=element_text(angle=90,hjust=0.5,vjust=0.5),axis.title.x=element_blank()) + ylab("%") + labs(fill='Income bin')  

getProportion <- function(columnA, columnB)
{
  return(data.frame(100*table(columnA,columnB)/apply(table(columnA,columnB),1,sum)))
}#getProportion

trainingData$lrFit <- predict(simpleLR,trainingData, type="response")
trainingData$lrFit <- ifelse(trainingData$lrFit<0.5,0,1)
trainingData$over50K <- as.integer(trainingData$incomeBin == "50000+.")
table(trainingData$lrFit,trainingData$over50K)


trainingData$rfPredictBin <- as.integer(as.character(trainingData$rfPredict)=="50000+.")
trainingData$incomeBin <- as.integer(as.character(trainingData$incomeBin)==" 50000+.")
pred <- prediction(trainingData$rfPredictBin,trainingData$over50K)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
