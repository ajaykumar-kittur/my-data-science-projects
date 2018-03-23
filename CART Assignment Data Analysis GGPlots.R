#monthly income
# distance form home
# total working years
# years at compan
#year since last promotion
#

##load data set


HR_Employee_Attrition_Data_ValidationSample <- read_excel("C:/Users/Sonia Padia/Desktop/R/Data Mining/Assignment CART/New folder/HR_Employee_Attrition_Data_ValidationSample.xlsx")
View(HR_Employee_Attrition_Data_ValidationSample)
Mydataset <- HR_Employee_Attrition_Data_ValidationSample
#data structure
str(Mydataset)
# data quality
summary(Mydataset)
#install package dplyr
#What proportoin of our employee is leaving based on numberof years of employee stay
StatusCount <- as.data.frame.matrix(Mydataset %>%
                               group_by(YearsAtCompany)%>%
                               select(Attrition)%>%
                               table())
StatusCount$TOTAL <- StatusCount$`0` + StatusCount$`1`
StatusCount$PercentTerminated <- StatusCount$`1`/(StatusCount$TOTAL)*100
StatusCount

mean(StatusCount$PercentTerminated)

## it ranges from 3.03 to 37%

##Where are terminations occuring
library(ggplot2)
ggplot() + geom_bar(aes(y=..count..,x=as.factor(Department), fill=as.factor(Attrition)),data = Mydataset, position=position_stack())

#From the graph above majority of the Attrition happened  in Research & Development hoever with respect to total percentage it appears that 
#comparatively more attrition occured in Sales


## below will plot data based on job satisfcation, number of years at company and Attrition
TerminatesData<- as.data.frame(Mydataset %>%
                                filter(Attrition=="1"))
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(YearsAtCompany),fill = as.factor(JobSatisfaction)),data=TerminatesData,position = position_stack())

##ggplot by salary hike and attrition
# attrition is impacted at high level due to salary hike
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(PercentSalaryHike),fill = as.factor(Attrition)),data=Mydataset,position = position_stack())

##ggplot by work life balance and attrition
# rating 2 and 3 for work life balance is greatly impacted 
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(WorkLifeBalance),fill = as.factor(Attrition)),data=Mydataset,position = position_stack())

##ggplot by overtime and attrition
# attrition is almost same across both the groups however with respect to total percentage, comparatively we can say attrition is getting impacted due to overtime as well
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(OverTime),fill = as.factor(Attrition)),data=Mydataset,position = position_stack())


##ggplot by overtime and attrition
# attrition is almost same across all the groups however with respect to toal percentage, comparatively we can say attrition is getting impacted due to overtime as well
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(RelationshipSatisfaction),fill = as.factor(Attrition)),data=Mydataset,position = position_stack())


##ggplot by years in current role and attrition
# attrition is mpre if employee is in ccurrent role since 0 or around 2 years or around 7 years 
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(YearsInCurrentRole),fill = as.factor(Attrition)),data=Mydataset,position = position_stack())

##ggplot by age and attrition
# attrition is maximum around age 30 
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(Age),fill = as.factor(Attrition)),data=Mydataset,position = position_stack())

##ggplot by business travel and attrition
# attrition is maximum - employees who travel rarely and frequently
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(BusinessTravel),fill = as.factor(Attrition)),data=Mydataset,position = position_stack())


##ggplot by Environmental Satisfaction and attrition
# attrition is almost same in all rating
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(EnvironmentSatisfaction),fill = as.factor(Attrition)),data=Mydataset,position = position_stack())

##ggplot by Gender and attrition
# attrition among male is more
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(Gender),fill = as.factor(Attrition)),data=Mydataset,position = position_stack())


##ggplot by Job Level and attrition
# attrition among level 1 and  2 and 3 is more
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(JobLevel),fill = as.factor(Attrition)),data=Mydataset,position = position_stack())


##ggplot by Job Satisfaction and attrition
# attrition among all category is almost same
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(JobSatisfaction),fill = as.factor(Attrition)),data=Mydataset,position = position_stack())

##ggplot by NumCompaniesWorked and attrition
# attrition among  category 1 is maximum 
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(NumCompaniesWorked),fill = as.factor(Attrition)),data=Mydataset,position = position_stack())


##ggplot by YearsWithCurrManager and attrition
# attrition for the yearswithmanager <= 0 is maximum 
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(YearsWithCurrManager),fill = as.factor(Attrition)),data=Mydataset,position = position_stack())




