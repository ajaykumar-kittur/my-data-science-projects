
##Finance And Risk Analytics Group Assignment

##Let's load the loanData set first
install.packages("DMwR")
install.packages("UBL")
install.packages("StatMeasures")

library(e1071)
library(xlsx)
library(ROCR)
library(caret)

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

loanData_test <- read.xlsx("C:/Users/Home/Downloads/off campus assignment/test.xlsx",1)
loanData_test$NumberOfDependents <- as.numeric(as.character(loanData_test$NumberOfDependents))
loanData_test = na.omit(loanData_test)
#loanData_test$NumberOfDependents[is.na(loanData_test$NumberOfDependents)] <- mean(loanData_test$NumberOfDependents, na.rm=TRUE)
loanData_test$SeriousDlqin2yrs <- as.factor(as.character(loanData_test$SeriousDlqin2yrs))
str(loanData_test)


#Build the model using SVM (Support Vector Machine)

##Lets try the SVM Kernel as  'Radial'
svmodel<-svm(SeriousDlqin2yrs~RevolvingUtilizationOfUnsecuredLines+DebtRatio+NumberOfOpenCreditLinesAndLoans+
               NumberOfDependents, data = loanData_smoted)
summary(svmodel)

##Confusion Matrix and Classification Error
pred <- predict(svmodel, loanData_smoted)

tab<-table(Predicted = pred, Actual = loanData_smoted$SeriousDlqin2yrs)

tab

confusionMatrix(pred, loanData_smoted$SeriousDlqin2yrs, positive = "1")

##Misclassification Error
1-sum(diag(tab))/sum(tab)

##For test dataset
pred2<-predict(svmodel, loanData_test)

tab2<-table(pred2, loanData_test$SeriousDlqin2yrs)

tab2

confusionMatrix(pred2, loanData_test$SeriousDlqin2yrs, positive = "1")

##Misclassification Error for Test Data
1-sum(diag(tab2))/sum(tab2)

####################################################################################
##Lets try the SVM Kernel as  'Linear'
svmodel<-svm(SeriousDlqin2yrs~RevolvingUtilizationOfUnsecuredLines+DebtRatio+NumberOfOpenCreditLinesAndLoans+
               NumberOfDependents, kernel = "linear", data = loanData_smoted)
summary(svmodel)

##Confusion Matrix and Classification Error
pred <- predict(svmodel, loanData_smoted)

tab<-table(Predicted = pred, Actual = loanData_smoted$SeriousDlqin2yrs)

tab

confusionMatrix(pred, loanData_smoted$SeriousDlqin2yrs, positive = "1")

##Misclassification Error
1-sum(diag(tab))/sum(tab)

##For test dataset
pred2<-predict(svmodel, loanData_test)

tab2<-table(pred2, loanData_test$SeriousDlqin2yrs)

tab2

confusionMatrix(pred2, loanData_test$SeriousDlqin2yrs, positive = "1")

##Misclassification Error
1-sum(diag(tab2))/sum(tab2)

####################################################################################
##Lets try the SVM Kernel as  'polynomial'
svmodel<-svm(SeriousDlqin2yrs~RevolvingUtilizationOfUnsecuredLines+DebtRatio+NumberOfOpenCreditLinesAndLoans+
               NumberOfDependents, kernel = "polynomial", data = loanData_smoted)
summary(svmodel)

##Confusion Matrix and Classification Error
pred <- predict(svmodel, loanData_smoted)

tab<-table(Predicted = pred, Actual = loanData_smoted$SeriousDlqin2yrs)

tab

confusionMatrix(pred, loanData_smoted$SeriousDlqin2yrs, positive = "1")

##Misclassification Error
1-sum(diag(tab))/sum(tab)

##For test dataset
pred2<-predict(svmodel, loanData_test)

tab2<-table(pred2, loanData_test$SeriousDlqin2yrs)

tab2

confusionMatrix(pred2, loanData_test$SeriousDlqin2yrs, positive = "1")

##Misclassification Error
1-sum(diag(tab2))/sum(tab2)

####################################################################################
##Lets try the SVM Kernel as  'sigmoid'
svmodel<-svm(SeriousDlqin2yrs~RevolvingUtilizationOfUnsecuredLines+DebtRatio+NumberOfOpenCreditLinesAndLoans+
               NumberOfDependents, kernel = "sigmoid", data = loanData_smoted)
summary(svmodel)

##Confusion Matrix and Classification Error
pred <- predict(svmodel, loanData_smoted)

tab<-table(Predicted = pred, Actual = loanData_smoted$SeriousDlqin2yrs)

tab

confusionMatrix(pred, loanData_smoted$SeriousDlqin2yrs, positive = "1")

##Misclassification Error
1-sum(diag(tab))/sum(tab)

##For test dataset
pred2<-predict(svmodel, loanData_test)

tab2<-table(pred2, loanData_test$SeriousDlqin2yrs)

tab2

confusionMatrix(pred2, loanData_test$SeriousDlqin2yrs, positive = "1")

##Misclassification Error
1-sum(diag(tab2))/sum(tab2)
###################################################################################

##Lets attempt to fine tune the model.
##Tuning the model
set.seed(100)

tmodel<-tune(svm, SeriousDlqin2yrs~RevolvingUtilizationOfUnsecuredLines+DebtRatio+
               NumberOfOpenCreditLinesAndLoans+NumberOfDependents, data = loanData_smoted, kernel = "radial",
             ranges = list(epsilon = seq(0,1,0.1), cost = 2^(5:7)))


summary(tmodel)

plot(tmodel)


##Best model

mysvmodel<- tmodel$best.model

summary(mysvmodel)

##Confusion Matrix and Classification Error
pred <- predict(mysvmodel, loanData_smoted)

tab<-table(Predicted = pred, Actual = loanData_smoted$SeriousDlqin2yrs)

tab

confusionMatrix(pred, loanData_smoted$SeriousDlqin2yrs, positive = "1")

##Misclassification Error
1-sum(diag(tab))/sum(tab)

##For test dataset
pred2<-predict(mysvmodel, loanData_test)

tab2<-table(pred2, loanData_test$SeriousDlqin2yrs)

tab2

confusionMatrix(pred2, loanData_test$SeriousDlqin2yrs, positive = "1")

##Misclassification Error
1-sum(diag(tab2))/sum(tab2)




