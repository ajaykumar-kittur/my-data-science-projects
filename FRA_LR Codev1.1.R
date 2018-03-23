##Finance And Risk Analytics Group Assignment Group # 2

library(xlsx)
library(DMwR)
library(graphics)
library(gdata)
library(dplyr)
library(UBL)
library(ggplot2)
library(glmnet)
library(caret)
library(ROCR)

#Read the Data file 
Perl64 <- "C:/Perl64/bin/perl.exe"
#loanData <- read.xlsx("C:/Users/Home/Downloads/off campus assignment/training.xlsx",1)
loanData <- read.xls(file.choose(),header = TRUE, perl = Perl64)
View(loanData)

summary(loanData)

str(loanData)

loanData$NumberOfDependents <- as.character(loanData$NumberOfDependents)
loanData$NumberOfDependents <- as.numeric(loanData$NumberOfDependents)

#There are some NAs in data replace them with mean values
loanData = na.omit(loanData)

summary(loanData)

str(loanData)

nrow(loanData)

# Check for any outliers
boxplot(loanData)

# Loan Data Debt ratio has just 1 extreme outlier which can be removed
boxplot(loanData$DebtRatio)

data_outlier <- loanData[which(loanData$DebtRatio >= 100000),]
data_outlier

View(loanData)

summary(loanData)

boxplot(loanData)

table(loanData$DebtRatio)


loanData$SeriousDlqin2yrs <- as.factor(as.character(loanData$SeriousDlqin2yrs))

#As this is a unbalanced dataset we need to balance this dataset using SMOTE function
loanData_smoted <- SMOTE(SeriousDlqin2yrs ~.,data = loanData, perc.over=400, k =11,  perc.under=125)
#You can verify the data is balanced
prop.table(table(loanData_smoted$SeriousDlqin2yrs))

##Plot of the Orignal Data
ggplot(loanData,aes(x=(Casenum) ,y=(SeriousDlqin2yrs),color=SeriousDlqin2yrs)) +
  geom_point(stat = "identity", position = "identity") + ggtitle("Orignal  Data")

##Plot of the SMOTE'd Data
ggplot(loanData_smoted,aes(x=(Casenum) ,y=(SeriousDlqin2yrs),color=SeriousDlqin2yrs)) +
  geom_point(stat = "identity", position = "identity") + ggtitle("SMOTE'd Data")

##Lets run the Logistic Regression
# Model with all variables
set.seed(10)
model1 <- glm(SeriousDlqin2yrs ~RevolvingUtilizationOfUnsecuredLines+DebtRatio+
                NumberOfOpenCreditLinesAndLoans+NumberOfDependents, data = loanData_smoted, family = binomial)

summary(model1)

#load the test data

loanData_test <- read.xlsx("C:/Users/Home/Downloads/off campus assignment/test.xlsx",1)
loanData_test$NumberOfDependents <- as.numeric(as.character(loanData_test$NumberOfDependents))
loanData_test$SeriousDlqin2yrs <- as.factor(as.character(loanData_test$SeriousDlqin2yrs))
str(loanData_test)


# Accuracy using Test Data
glm_prob <- predict.glm(model1,loanData_test,type="response")

#make predictions
##.first create vector to hold predictions (we know 0 refers to neg now)
glm_predict <- rep("neg",nrow(loanData_test))
glm_predict[glm_prob>0.5] <- "pos"
#confusion matrix
table(pred=glm_predict,true=loanData_test$SeriousDlqin2yrs)

ROCRpred <- prediction(glm_prob,loanData_test$SeriousDlqin2yrs)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))
abline(a = 0, b = 1)

#convert training data to matrix format
x <- model.matrix( SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines + 
                     DebtRatio + NumberOfOpenCreditLinesAndLoans + NumberOfDependents,loanData_smoted)
#convert class to numerical variable
y <- ifelse(loanData_smoted$SeriousDlqin2yrs=="1",1,0)

# Perform the ridge regression
set.seed(10)
cv.out <- cv.glmnet(x,y,alpha=0,family="binomial",type.measure = "mse" )

plot(cv.out)
#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=lambda_1se)

x_test <- model.matrix(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines + 
                         DebtRatio + NumberOfOpenCreditLinesAndLoans + NumberOfDependents,loanData_test)
#predict class, type="class"
ridge_prob <- predict(cv.out,newx = x_test,s=lambda_1se,type="response")
#translate probabilities to predictions
ridge_predict <- rep("neg",nrow(loanData_test))
ridge_predict[ridge_prob>0.5] <- "pos"
#confusion matrix
table(pred=ridge_predict,true=loanData_test$SeriousDlqin2yrs)

# Perform the LASSO regression

cv.out <- cv.glmnet(x,y,alpha=1,family="binomial",type.measure = "mse" )

plot(cv.out)
#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=lambda_1se)

#predict class, type="class"
lasso_prob <- predict(cv.out,newx = x_test,s=lambda_1se,type="response")
#translate probabilities to predictions
lasso_predict <- rep("neg",nrow(loanData_test))
lasso_predict[lasso_predict>0.5] <- "pos"
#confusion matrix
table(pred=lasso_predict,true=loanData_test$SeriousDlqin2yrs)

