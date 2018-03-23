######DM Assignment#########

setwd ("G:/Grreat lakes/Data Mining/Group Project")
getwd()
library("caret")
library("rpart")
library("rpart.plot")
library(caretEnsemble)
library(tibble)
## Data Import
CTDF.dev <- read.table("HR_Employee_Attrition_Data.csv", sep = ",", header = T)
str(CTDF.dev)
CTDF.dev$Attrition=ifelse(CTDF.dev$Attrition=="Yes",1,0)
str(CTDF.dev)
CTDF.dev

######Exploratory Data Analysis#######
summary(CTDF.dev)
#removing categorical varible, for categorical variabe we can use chi square
cormtrix<- CTDF.dev[,c(-2,-3,-5,-8,-12,-16,-18,-22,-23)]
str(cormtrix)
cor(cormtrix)
# Result
# age and income is +corelated,age and joblevel,TotalWorkingYears,YearsAtCompany and YearsWithCurrManager
# -ve 



library("Information")
IV <-create_infotables(data=CTDF.dev, y="Attrition",bins=10,parallel = FALSE)
#install.packages("Information")
IV_Value = data.frame(IV$Summary)
IV$Summary
str(IV_Value)
print(IV_Value)
data.frame(IV$Tables)


IV_Value= subset(IV_Value,IV_Value$IV>=0.1)
nrow(IV_Value)
IV_Value= subset(IV_Value,IV_Value$IV<=0.5)

nrow(IV_Value)

#0.1 to 0.5

head(mydata2)
mydata2=  CTDF.dev[,  IV_Value$Variable]
mydata2$Attrition=CTDF.dev$Attrition
#mydata2=cbind(mydata$Attrition,mydata2)


names(mydata2)
CTDF.dev<-mydata2
nrow(CTDF.dev)




# Normality analysis via Histogram, we check for numeric data
par(mfrow= c(2,4))

hist(CTDF.dev$Age, main ="Age",xlab="") 
#hist(CTDF.dev$BusinessTravel, main = "BusinessTravel",xlab="")
hist(CTDF.dev$DailyRate, main="DailyRate",xlab="")
hist(CTDF.dev$DistanceFromHome, main ="DistanceFromHome",xlab="")
hist(CTDF.dev$Education, main = "Education",xlab="")
#hist(CTDF.dev$EducationField, main = "EducationField",xlab="")
hist(CTDF.dev$EnvironmentSatisfaction, main = "EnvironmentSatisfaction",xlab="")
#hist(CTDF.dev$Gender, main = "Gender",xlab="")
hist(CTDF.dev$HourlyRate, main="HourlyRate",xlab="")
hist(CTDF.dev$JobInvolvement, main ="JobInvolvement",xlab="")
hist(CTDF.dev$JobLevel, main = "JobLevel",xlab="")
hist(CTDF.dev$JobSatisfaction, main="JobSatisfaction",xlab="")
hist(CTDF.dev$MonthlyIncome, main ="MonthlyIncome",xlab="")
hist(CTDF.dev$NumCompaniesWorked, main = "NumCompaniesWorked",xlab="")
#hist(CTDF.dev$Over18, main="Over18",xlab="")
hist(CTDF.dev$PercentSalaryHike, main ="PercentSalaryHike",xlab="")
hist(CTDF.dev$PerformanceRating, main = "PerformanceRating",xlab="")

hist(CTDF.dev$RelationshipSatisfaction, main="RelationshipSatisfaction",xlab="")
hist(CTDF.dev$StockOptionLevel, main ="RelationshipSatisfaction",xlab="")
hist(CTDF.dev$TotalWorkingYears, main = "TotalWorkingYears",xlab="")


hist(CTDF.dev$TrainingTimesLastYear, main ="TrainingTimesLastYear",xlab="")
hist(CTDF.dev$WorkLifeBalance, main = "WorkLifeBalance",xlab="")
hist(CTDF.dev$YearsAtCompany, main="YearsAtCompany",xlab="")
hist(CTDF.dev$YearsInCurrentRole, main ="YearsInCurrentRole",xlab="")
hist(CTDF.dev$YearsSinceLastPromotion, main = "YearsSinceLastPromotion",xlab="")
hist(CTDF.dev$YearsWithCurrManager, main ="YearsWithCurrManager",xlab="")


title("
      Histogram to check data distribution - HR_Employee_Attrition_Data", outer=TRUE)


##Finding outlier and detecting it
par(mfrow=c(2,4))
p1<-boxplot(CTDF.dev$Age, main ="Age",col = "orange") 
#hist(CTDF.dev$BusinessTravel, main = "BusinessTravel",xlab="")
p2<-boxplot(CTDF.dev$DailyRate, main="DailyRate",col = "orange")
p3<-boxplot(CTDF.dev$DistanceFromHome, main ="DistanceFromHome",col = "orange")
p4<-boxplot(CTDF.dev$Education, main = "Education",xlab="")
#hist(CTDF.dev$EducationField, main = "EducationField",xlab="")
p5<-boxplot(CTDF.dev$EnvironmentSatisfaction, main = "EnvironmentSatisfaction",col = "orange")
#hist(CTDF.dev$Gender, main = "Gender",xlab="")
p6<-boxplot(CTDF.dev$HourlyRate, main="HourlyRate",col = "orange")
p7<-boxplot(CTDF.dev$JobInvolvement, main ="JobInvolvement",col = "orange")
p8<-boxplot(CTDF.dev$JobLevel, main = "JobLevel",col = "orange")
p9<-boxplot(CTDF.dev$JobSatisfaction, main="JobSatisfaction",col = "orange")
p10<-boxplot(CTDF.dev$MonthlyIncome, main ="MonthlyIncome",col = "orange")
p11<-boxplot(CTDF.dev$NumCompaniesWorked, main = "NumCompaniesWorked",col = "orange")
#hist(CTDF.dev$Over18, main="Over18",xlab="")
p12<-boxplot(CTDF.dev$PercentSalaryHike, main ="PercentSalaryHike",col = "orange")
p13<-boxplot(CTDF.dev$PerformanceRating, main = "PerformanceRating",col = "orange")
p14<-boxplot(CTDF.dev$RelationshipSatisfaction, main="RelationshipSatisfaction",col = "orange")
p15<-boxplot(CTDF.dev$StockOptionLevel, main ="RelationshipSatisfaction",col = "orange")
p16<-boxplot(CTDF.dev$TotalWorkingYears, main = "TotalWorkingYears",col = "orange")


p17<-boxplot(CTDF.dev$TrainingTimesLastYear, main ="TrainingTimesLastYear",col = "orange")
p18<-boxplot(CTDF.dev$WorkLifeBalance, main = "WorkLifeBalance",col = "orange")
p19<-boxplot(CTDF.dev$YearsAtCompany, main="YearsAtCompany",col = "orange")
p20<-boxplot(CTDF.dev$YearsInCurrentRole, main ="YearsInCurrentRole",col = "orange")
p21<-boxplot(CTDF.dev$YearsSinceLastPromotion, main = "YearsSinceLastPromotion",col = "orange")
p22<-boxplot(CTDF.dev$YearsWithCurrManager, main ="YearsWithCurrManager",col = "orange")
title("
      Boxplot Analysis -HR_Employee_Attrition_Data ", outer=TRUE)

# Outlier Analysis - Varaiable


outlier_upper=function(x){
  q = quantile(x)
  names(q) = NULL
  q1 = q[2]
  q3 = q[4]
  QR = q3-q1
  return(q3+1.5*QR);
}

outlier_lower=function(x){
  q = quantile(x)
  names(q) = NULL
  q1 = q[2]
  q3 = q[4]
  QR = q3-q1
  return(q1-1.5*QR);
}

# outlier limits validation ------------------
str(CTDF.dev)
age_upper = outlier_upper(CTDF.dev$Age)
age_lower = outlier_lower(CTDF.dev$Age)
#businesstravel_upper = outlier_upper(CTDF.dev$BusinessTravel)
#businesstravel_lower = outlier_lower(CTDF.dev$BusinessTravel)
dailyrate_upper = outlier_upper(CTDF.dev$DailyRate)
dailyrate_lower = outlier_lower(CTDF.dev$DailyRate)
#JobRole_upper = outlier_upper(CTDF.dev$JobRole)
#JobRole_lower = outlier_lower(CTDF.dev$JobRole)
MonthlyIncome_upper = outlier_upper(CTDF.dev$MonthlyIncome)
MonthlyIncome_lower = outlier_lower(CTDF.dev$MonthlyIncome)
YearsAtCompany_upper = outlier_upper(CTDF.dev$YearsAtCompany)
YearsAtCompany_lower = outlier_lower(CTDF.dev$YearsAtCompany)
YearsSinceLastPromotion_upper = outlier_upper(CTDF.dev$YearsSinceLastPromotion)
YearsSinceLastPromotion_lower = outlier_lower(CTDF.dev$YearsSinceLastPromotion)

# Outlier data
#CTDF.dev[CTDF.dev$DailyRate>dailyrate_upper | CTDF.dev$DailyRate<dailyrate_lower , ]
#CTDF.dev<-CTDF.dev[CTDF.dev$MonthlyIncome>MonthlyIncome_upper | CTDF.dev$MonthlyIncome<MonthlyIncome_lower , ]
#CTDF.dev[CTDF.dev$YearsAtCompany>YearsAtCompany_upper | CTDF.dev$YearsAtCompany<YearsAtCompany_lower , ]
#CTDF.dev[CTDF.dev$YearsSinceLastPromotion> YearsSinceLastPromotion_upper | CTDF.dev$YearsSinceLastPromotion< YearsSinceLastPromotion_lower , ]

str(CTDF.dev)

#CTDF.dev = subset( CTDF.dev, CTDF.dev$<=dailyrate_upper & CTDF.dev$DailyRate>=dailyrate_lower)
CTDF.dev = subset( CTDF.dev, CTDF.dev$MonthlyIncome<=MonthlyIncome_upper & CTDF.dev$MonthlyIncome>=MonthlyIncome_lower)
CTDF.dev = subset( CTDF.dev, CTDF.dev$YearsAtCompany<=YearsAtCompany_upper & CTDF.dev$YearsAtCompany>=YearsAtCompany_lower)
#CTDF.dev = subset( CTDF.dev, CTDF.dev$y<=YearsSinceLastPromotion_upper & CTDF.dev$YearsSinceLastPromotion>=YearsSinceLastPromotion_lower)
CTDF.dev
nrow(CTDF.dev)
row.names(CTDF.dev)= 1:nrow(CTDF.dev)



#######
library(caret)
# create a list of 70% of the rows in the original dataset we can use for training
training_data <- createDataPartition(CTDF.dev$Attrition, p=0.70, list=FALSE)
?createDataPartition
training_data
# select 30% of the data for validation
validation_data <- CTDF.dev[-training_data,]
nrow(validation_data)
head(validation_data)
# use the remaining 70% of data to training and testing the models
training_data<-CTDF.dev[training_data,]
head(training_data) 
nrow(training_data)
#CTDF.dev <- CTDF.dev[training_data,]
c(nrow(validation_data),nrow(training_data))
ttr.dev<-training_data
tt1.val<-validation_data
View(tt1.val)