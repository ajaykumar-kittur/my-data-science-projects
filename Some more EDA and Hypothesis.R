#set working directory
setwd("G:/Grreat lakes/Data Mining/Group Project/assignment")
getwd()

# Load Data
attritiondata <- read.csv("HR_Employee_Attrition_Data.csv")
View(attritiondata)
str(attritiondata)
attritiondata$Attrition=ifelse(attritiondata$Attrition=="Yes",1,0)
#### Install Packages:
install.packages("Hmisc")

####### Exploratory Data Analysis #############
# 1. Data Description ********


library(Hmisc)
## n, nmiss, unique, mean, 5,10,25,50,75,90,95th percentiles 
## 5 lowest and 5 highest scores
describe(attritiondata)

# 2. Box plots to visualize the data ***********

boxplot(attritiondata$TotalWorkingYears~attritiondata$Attrition,main="Boxplot by Total Working Years",ylab="Years of Experience")
boxplot(attritiondata$Age, main="Employee Age")
boxplot(attritiondata$MonthlyIncome~attritiondata$Department, data=attritiondata, main = "Department wise Monthly Income")
boxplot(attritiondata$MonthlyIncome~attritiondata$Gender, data=attritiondata, main = "Gender wise Monthly Income")


##Regression Model for continuous variables
set.seed(1)
index <- sample(1:nrow(attritiondata),200)
SampleSet <- attritiondata[index,]
str(SampleSet)
Overtime.matrix <- model.matrix(~ OverTime - 1, data = SampleSet)
SampleSet <- data.frame(SampleSet, Overtime.matrix)
View(SampleSet)
MaritalStatus.matrix <- model.matrix(~ MaritalStatus - 1, data = SampleSet)
SampleSet <- data.frame(SampleSet, MaritalStatus.matrix)

BusinessTravel.matrix <- model.matrix(~ BusinessTravel -1, data= SampleSet)
SampleSet <- data.frame(SampleSet, BusinessTravel.matrix)
fit <- lm(SampleSet$YearsAtCompany ~ SampleSet$DistanceFromHome + SampleSet$MonthlyIncome + SampleSet$PercentSalaryHike+SampleSet$TotalWorkingYears + SampleSet$YearsInCurrentRole+SampleSet$YearsSinceLastPromotion + SampleSet$YearsWithCurrManager,data = SampleSet)
summary(fit)
vif(fit)

plot(fit)

#vif >4 multicollinear

str(SampleSet)
aov(Attrition~BusinessTravel,data=SampleSet)->ft
summary(ft)



fit <- lm(SampleSet$Attrition ~ SampleSet$DistanceFromHome + SampleSet$MonthlyIncome + SampleSet$PercentSalaryHike+SampleSet$TotalWorkingYears + SampleSet$YearsInCurrentRole+SampleSet$YearsSinceLastPromotion + SampleSet$YearsWithCurrManager,data = SampleSet)

#summary statistics
attritiondata$Attrition=ifelse(attritiondata$Attrition=="Yes",1,0)
#train.data$Attrition=ifelse(train.data$Attrition=="Yes",1,0)
#test.data$Attrition=ifelse(test.data$Attrition=="Yes",1,0)

# Normality analysis via Histogram

par(mfrow= c(2,4))
hist(attritiondata$Age, main ="Age",xlab="") 
hist(attritiondata$MonthlyIncome, main = "Monthly Income",xlab="")
hist(attritiondata$MonthlyRate, main="Monthly Rate",xlab="")
hist(attritiondata$DistanceFromHome, main ="Distance from home",xlab="")
hist(attritiondata$PercentSalaryHike, main = "PercentSalaryHike",xlab="")
hist(attritiondata$TotalWorkingYears, main = "TotalWorkingYears",xlab="")
hist(attritiondata$YearsInCurrentRole, main = "Years in current Role",xlab="")
hist(attritiondata$RH, main = "RH",xlab="")
#title("Histogram to check data distribution - RK Puram", outer=TRUE)

#Outlier checking using Boxplot ********************
#attritiondata$City = "Delhi_attritiondata"
library("ggplot2")

#Read attrition data from csv
library(foreign)
library("scales")

ggplot(attritiondata, aes(x = JobRole)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())

#Hypothesis 2: Job levels do not have any impact on attrition
ggplot(attritiondata, aes(x = JobLevel)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())

#Hypothesis 3: Overtime has direct impact on attrition
ggplot(attritiondata, aes(x = OverTime)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())




## Department Vs Attrition
tabdep=table(attritiondata$Department,attritiondata$Attrition) 
barplot(tabdep,beside=T,legend=T,main="Department Vs Attrition" ,ylab="Attrition")

## Gender Vs Attrition

tabgender=table(attritiondata$Gender,attritiondata$Attrition) 
barplot(tabgender,beside=T,legend=T,main="Barplot of Gender" ,ylab="Attrition")


##########
#3. Hypothesis

#Hypothesis 1: Some job roles have high attrition rate
tabjobrole = table(attritiondata$JobRole,attritiondata$Attrition)
tabjobrole
chisq.test(tabjobrole)

# Hypothesis 2: Attrition  is not dependent on Over time
tabovertime =table(attritiondata$OverTime, attritiondata$Attrition)
tabovertime
chisq.test(tabovertime)

# Hypothesis 3: Attrition  is not dependent on education
tabEducation =table(attritiondata$Education, attritiondata$Attrition)
tabEducation
chisq.test(tabEducation)

tabEducation =table(attritiondata$EducationField, attritiondata$Attrition)
tabEducation
chisq.test(tabEducation)

# Hypothesis 5: Attrition  is not dependent on Gender
tabGender =table(attritiondata$Gender, attritiondata$Attrition)
tabGender
chisq.test(tabGender)

# Hypothesis 5: Attrition  is not dependent on Gender
tabMaritalStatus =table(attritiondata$MaritalStatus, attritiondata$Attrition)
tabMaritalStatus
chisq.test(tabMaritalStatus)
MaritalStatus