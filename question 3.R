crowd_data <- read_xlsx("~/Downloads/Crowdfunding_data.xlsx")
crowd_data <- crowd_data[,c("success","state","goal","fb_total_likes","category","unique_visitors","start_month","end_month","prior_campaigns","num_perks")]

crowd_data$state <-as.factor(crowd_data$state)
crowd_data$category <-as.factor(crowd_data$category)
crowd_data$start_month <-as.factor(crowd_data$start_month)
crowd_data$end_month <-as.factor(crowd_data$end_month)
crowd_data$prior_campaigns <-as.factor(crowd_data$prior_campaigns)
crowd_data$num_perks <-as.factor(crowd_data$num_perks)

library(caTools)
set.seed(100)

sample = sample.split(crowd_data, SplitRatio = 0.6)
CDtrain<- subset(crowd_data,sample == TRUE)
CDtest <- subset(crowd_data, sample == FALSE)

library("rpart")
library("rpart.plot")
library("caret")
library("e1071")

tree_1<-rpart(success ~ ., 
              data = CDtrain, method = "class", control = rpart.control(minsplit = 200))
rpart.plot(tree_1,type=1, extra = 2,under = TRUE)
title(sub = "Classification Tree with minsplit = 200")

tree_2<-rpart(success ~ ., 
              data = CDtrain, method = "class", control = rpart.control(minsplit = 100))
rpart.plot(tree_2,type=1, extra = 2,under = TRUE)
title(sub = "Classification Tree with minsplit = 100")

tree_3<-rpart(success ~ ., 
              data = CDtrain, method = "class", control = rpart.control(minsplit = 50))
rpart.plot(tree_3,type=1, extra = 2,under = TRUE)
title(sub = "Classification Tree with minsplit = 50")

pred1<- predict(tree_1, CDtest, type="class")
confusionMatrix(pred1, CDtest$success, dnn = c("Predicted","Actual"))

pred2<- predict(tree_2, CDtest, type="class")
confusionMatrix(pred2, CDtest$success, dnn = c("Predicted","Actual"))

pred3<- predict(tree_3, CDtest, type="class")
confusionMatrix(pred3, CDtest$success, dnn = c("Predicted","Actual"))
