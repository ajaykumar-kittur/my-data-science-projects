#import data set

HR_Employee_Attrition_Data_RF1 <- read_excel("C:/Users/Sonia Padia/Desktop/R/Data Mining/Assignment Random Forest/HR_Employee_Attrition_Data_RF.xlsx")
View(HR_Employee_Attrition_Data_RF1)

#count rows and columns
TotalRows <- c(nrow(HR_Employee_Attrition_Data_RF1))
TotalRows
TotalCols <- c(ncol(HR_Employee_Attrition_Data_RF1))
TotalCols


library(caTools)
HR_Employee_Attrition_Data_RF1$Attrition=ifelse(HR_Employee_Attrition_Data_RF1$Attrition=="Yes",1,0)

summary(HR_Employee_Attrition_Data_RF1)


#convert char into factor var
names <- c(3,5,8,12,16,18,22,23)
HR_Employee_Attrition_Data_RF1[,names] <- lapply(HR_Employee_Attrition_Data_RF1[,names] , factor)

# partition data into 70:30 ratio
smp_size <- floor(0.70 * TotalRows)

#HR_Employee_Attrition_Data_RF_updated <- HR_Employee_Attrition_Data_RF[,c(-9,-10,-22,-27)]

train_ind <- sample(seq_len(nrow(HR_Employee_Attrition_Data_RF1)), size = smp_size)

train_data <- HR_Employee_Attrition_Data_RF1[train_ind, ]
test_data <- HR_Employee_Attrition_Data_RF1[-train_ind, ]
View(train_data)

# convert attrition to numeric
#levels(train_data$Attrition) <- c(0,1)


library(randomForest)
## Calling syntax to build the Random Forest
RF <- randomForest(as.factor(train_data$Attrition) ~ ., 
                   data = train_data[,c(-9,-10,-22,-27)], 
                   ntree=22, mtry = 7, nodesize = 125,
                   importance=TRUE)

print(RF)

plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest train_data")

RF$err.rate


#tune to determine m

## Tuning Random Forest
tRF_Attrition <- tuneRF(x = train_data[,c(-2,-9,-10,-22,-27)], 
              y=as.factor(train_data$Attrition),
              mtryStart = 3, 
              ntreeTry=22, 
              stepFactor = 1.5, 
              improve = 0.001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 125, 
              importance=TRUE
)

# ideal m value to reduce error is 13



# previous model
impVar <- round(randomForest::importance(RF), 2)
impVar[order(impVar[,3], decreasing=TRUE),]

#final model after tuning m 
impVar <- round(randomForest::importance(tRF_Attrition), 2)
impVar[order(impVar[,3], decreasing=TRUE),]


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


train_data$predict.class <- predict(tRF_Attrition, train_data, type="class")
train_data$predict.score <- predict(tRF_Attrition, train_data, type="prob")
head(train_data)

## deciling
train_data$deciles <- decile(train_data$predict.score[,2])




## Ranking code
library(data.table)
tmp_DT = data.table(train_data)
rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = sum(Attrition), 
  cnt_non_resp = sum(Attrition == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)






## Scoring syntax
test_data$predict.class <- predict(tRF_Attrition, test_data, type="class")
test_data$predict.score <- predict(tRF_Attrition, test_data, type="prob")
with(test_data, table(Attrition, predict.class))

test_data$deciles <- decile(test_data$predict.score[,2])
tmp_DT = data.table(test_data)
h_rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_resp = sum(Attrition), 
  cnt_non_resp = sum(Attrition == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round(h_rank$cnt_resp * 100 / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),2);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);
View(h_rank)





