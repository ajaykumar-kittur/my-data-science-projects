##Finance And Risk Analytics Group Assignment

##Let's load the loanData set first

library(randomForest)
library(xlsx)

# Read the data 
loanData <- read.xlsx("C:/Users/Home/Downloads/off campus assignment/training.xlsx",1)
View(loanData)

summary(loanData)

str(loanData)

loanData$NumberOfDependents <- as.character(loanData$NumberOfDependents)
loanData$NumberOfDependents <- as.numeric(loanData$NumberOfDependents)

#There are some NAs in data replace them with mean values
loanData = na.omit(loanData)
str(loanData)

nrow(loanData)

# Check for any outliers
boxplot(loanData)

# Loan Data Debt ratio has just 1 extreme outlier which can be removed
boxplot(loanData$DebtRatio)

data_outlier <- loanData[which(loanData$DebtRatio >= 100000),]
data_outlier
loanData = loanData[-data_outlier$Casenum,]

View(loanData)

summary(loanData)

boxplot(loanData)

table(loanData$DebtRatio)

#Since Data is Unbalanced balance the data using SMOTE function
require(DMwR)
loanData$SeriousDlqin2yrs <- as.factor(as.character(loanData$SeriousDlqin2yrs))

#As this is a unbalanced dataset we need to balance this dataset using SMOTE function
loanData_smoted <- SMOTE(SeriousDlqin2yrs ~.,data = loanData, perc.over=400, k = 11,  perc.under=125)
#You can verify the data is balanced
prop.table(table(loanData_smoted$SeriousDlqin2yrs))


#Build the model using Random Forest
library(randomForest)
set.seed(222)
str(loanData_smoted)
rf <- randomForest(SeriousDlqin2yrs~RevolvingUtilizationOfUnsecuredLines+DebtRatio+
                     NumberOfOpenCreditLinesAndLoans+NumberOfDependents, data=loanData_smoted)
print(rf)
# Plot the OOB Error rate
plot(rf)
# Check the prediction accuracy with original RF model on train data
library(caret)
p1<-predict(rf, loanData_smoted)
confusionMatrix(p1, loanData_smoted$SeriousDlqin2yrs)

loanData_test <- read.xlsx("C:/Users/Home/Downloads/off campus assignment/test.xlsx",1)
loanData_test$NumberOfDependents <- as.numeric(as.character(loanData_test$NumberOfDependents))
#loanData_test = na.omit(loanData_test)
loanData_test$SeriousDlqin2yrs <- as.factor(as.character(loanData_test$SeriousDlqin2yrs))
str(loanData_test)


# Check the prediction accuracy with  RF model on test data
p4<-predict(rf, loanData_test)
confusionMatrix(p4, loanData_test$SeriousDlqin2yrs, positive = "1")


#Tune the RF model
tRF <- tuneRF(loanData_smoted[,-6], loanData_smoted[,6],
              stepFactor = 0.5, plot = TRUE,
              ntreeTry = 450,
              trace = TRUE,
              improve = 0.001)
# put the tune parameters in the RF model
tunedRf <- randomForest(SeriousDlqin2yrs~RevolvingUtilizationOfUnsecuredLines+DebtRatio+
                          NumberOfOpenCreditLinesAndLoans+NumberOfDependents, data=loanData_smoted, ntree=400, mtry=2, importance = TRUE, proximity=TRUE)

print(tunedRf)


# Check the prediction accuracy with tuned RF model on train data
p2<-predict(tunedRf, loanData_smoted)
confusionMatrix(p2, loanData_smoted$SeriousDlqin2yrs, positive = "1")
str(loanData_test)

# Check the prediction accuracy with tuned RF model on test data
p3<-predict(tunedRf, loanData_test)
confusionMatrix(p3, loanData_test$SeriousDlqin2yrs, positive = "1")

##No of nodes for the trees
hist(treesize(tunedRf), main = "Number of Nodes for Trees", col = "blue")

##Variable Importance
varImpPlot(tunedRf, sort = T, main = "Top Variables by importance")

importance(tunedRf)

varUsed(tunedRf)

##Partial Dependence Plot
partialPlot(tunedRf, loanData_smoted, DebtRatio, "1")

partialPlot(tunedRf, loanData_smoted, DebtRatio, "0")

##Extract Single Tree from Forest

getTree(tunedRf, 1, labelVar = TRUE)

##Multi-Dimensional Plot of Proximity Matrix. Here MDS plot is for academic purposes,
##we cannot use a MDS plot here as no of levels in a predictor variable needs be atleast 3, 
##we have in our case only 2
MDSplot(tunedRf, loanData_smoted$SeriousDlqin2yrs, main = "Training Data")

MDSplot(tunedRf, loanData_test$SeriousDlqin2yrs, main = "Test Data")

#Scoring Syntax
loanData_smoted$predict.class <- predict(tunedRf, loanData_smoted, type="class")
loanData_smoted$predict.score <- predict(tunedRf, loanData_smoted, type="prob")
head(loanData_smoted)

library(StatMeasures)
loanData_smoted$deciles <- decile(loanData_smoted$predict.score[,2])

## Ranking code
levels(loanData_smoted)=c(1,0)
loanData_smoted$SeriousDlqin2yrs = as.numeric(as.character(loanData_smoted$SeriousDlqin2yrs))
loanData_smoted$SeriousDlqin2yrs

library(data.table)
tmp_DT = data.table(loanData_smoted)
rank <- tmp_DT[, list(
  cnt = length(SeriousDlqin2yrs), 
  cnt_resp = sum(SeriousDlqin2yrs), 
  cnt_non_resp = sum(SeriousDlqin2yrs == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)


library(ROCR)
pred <- prediction(loanData_smoted$predict.score[,2], loanData_smoted$SeriousDlqin2yrs)
perf <- performance(pred, "tpr", "fpr")
plot(perf, main = "AUC - Train", colorize = T)
abline(a = 0, b = 1)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)



#install.packages("ineq")
library(ineq)
gini = ineq(loanData_smoted$predict.score[,2], type="Gini")
with(loanData_smoted, table(SeriousDlqin2yrs, predict.class))
auc
KS
gini


#Scoring Syntax
loanData_test$predict.class <- predict(tunedRf, loanData_test, type="class")
loanData_test$predict.score <- predict(tunedRf, loanData_test, type="prob")
head(loanData_test)

loanData_test$deciles <- decile(loanData_test$predict.score[,2])

## Ranking code
levels(loanData_test)=c(1,0)
loanData_test$SeriousDlqin2yrs = as.numeric(as.character(loanData_test$SeriousDlqin2yrs))
loanData_test$SeriousDlqin2yrs

library(data.table)
tmp_DT = data.table(loanData_test)
rank <- tmp_DT[, list(
  cnt = length(SeriousDlqin2yrs), 
  cnt_resp = sum(SeriousDlqin2yrs), 
  cnt_non_resp = sum(SeriousDlqin2yrs == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)


library(ROCR)
pred <- prediction(loanData_test$predict.score[,2], loanData_test$SeriousDlqin2yrs)
perf <- performance(pred, "tpr", "fpr")
plot(perf, main = "AUC - test", colorize = T)
abline(a = 0, b = 1)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(loanData_test$predict.score[,2], type="Gini")
with(loanData_test, table(SeriousDlqin2yrs, predict.class))
auc
KS
gini

## deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}
