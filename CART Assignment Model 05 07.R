## installing rpart package for CART
## install.packages("rpart")
## install.packages("rpart.plot")

HR_Employee_Attrition_Data <- read_excel("C:/Users/Sonia Padia/Desktop/R/Data Mining/Assignment CART/DM Assignment/HR_Employee_Attrition_Data.xlsx")
View(HR_Employee_Attrition_Data)


TotalRows <- c(nrow(HR_Employee_Attrition_Data))
library(caTools)

# partition data into 70:30 ratio
smp_size <- floor(0.70 * TotalRows)

set.seed(123)
train_ind <- sample(seq_len(nrow(HR_Employee_Attrition_Data)), size = smp_size)

train <- HR_Employee_Attrition_Data[train_ind, ]
test <- HR_Employee_Attrition_Data[-train_ind, ]

View(train)
## loading the library
trainingdata_Updated <- train[,c(-9,-10,-22,-27)]
View(trainingdata_Updated)

library(rpart)
library(rpart.plot)
c(nrow(trainingdata_Updated))


## Target Rate 
sum(trainingdata_Updated$Attrition)/2058


## setting the control paramter inputs for rpart
r.ctrl = rpart.control(minsplit=82, minbucket = 41, cp = 0, xval = 2)
HR_EmployeeAttrition_Model1 <- rpart(formula = Attrition ~ ., data = trainingdata_Updated, method = "class", control = r.ctrl)
HR_EmployeeAttrition_Model1


## install.packages("rattle")
## install.packages("RcolorBrewer")
library(rattle)
library(RColorBrewer)
library(rpart)
fancyRpartPlot(HR_EmployeeAttrition_Model1)

## to find how the tree performs
printcp(HR_EmployeeAttrition_Model1)
plotcp(HR_EmployeeAttrition_Model1)


##rattle()
## Pruning Code
ptree<- prune(HR_EmployeeAttrition_Model1, cp= 0.0085 ,"CP")
printcp(ptree)
fancyRpartPlot(ptree, uniform=TRUE,  main="Pruned Classification Tree")



## Let's use rattle to see various model evaluation measures
##rattle()

View(train)
## Scoring syntax
## you can pass type = probability
train$predict.class <- predict(HR_EmployeeAttrition_Model1, train, type="class")
train$predict.score <- predict(HR_EmployeeAttrition_Model1, test)

View(train)
head(train)


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

class(train$predict.score)
## deciling
train$deciles <- decile(train$predict.score[,2])
View(train)

## Ranking code
##install.packages("data.table")
library(data.table)
tmp_DT = data.table(test)
rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = sum(Attrition), 
  cnt_non_resp = sum(Attrition == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_perct_resp <- round(rank$cum_resp * 100 / sum(rank$cnt_resp),2);
rank$cum_perct_non_resp <- round(rank$cum_non_resp * 100 / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_perct_resp - rank$cum_perct_non_resp);

View(rank)



