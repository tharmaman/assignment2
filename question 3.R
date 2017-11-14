crowd_data <- read_xlsx("~/Downloads/Crowdfunding_data.xlsx")
summary(crowd_data)
crowd_data <- crowd_data[,c("success","city","state","goal","fb_total_likes","category","unique_visitors","start_month","end_month","prior_campaigns","num_perks")]
str(crowd_data)

library(caTools)
set.seed(2)

sample = sample.split(crowd_data, SplitRatio = 0.6)
CDtrain<- subset(crowd_data,sample == TRUE)
CDtest <- subset(crowd_data, sample ==FALSE)

install.packages("rpart")
library(rpart)

install.packages("rpart.plot")
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



