# Importing crowd_data
crowd_data <- crowd_data <- read.csv("~/Downloads/Crowdfunding_data.xlsx", head = TRUE)

# Prepping data for Classification Tree Analysis
# dependent variables: 
# success (dichotomous variable) 

# predictor variables:
# category, state, goal, fb_total_likes, num_perks
# total_visitors, start_month, end_month, prior_campaigns, 


# Decided not to include the predictor "city" after realizing it was insignificant and 
# it caused to much trouble when making the trees

crowd_data <- crowd_data[,c("success","state","goal","fb_total_likes","category","unique_visitors","start_month","end_month","prior_campaigns","num_perks")]

# Turning categorical variables into factors
crowd_data$state <-as.factor(crowd_data$state)
crowd_data$category <-as.factor(crowd_data$category)

# Splitting data set in 60% training data and 40% testing data
library(caTools)
set.seed(100)
sample = sample.split(crowd_data, SplitRatio = 0.6)
CDtrain<- subset(crowd_data,sample == TRUE)
CDtest <- subset(crowd_data, sample == FALSE)

# Installing packages for trees and confusion matrix testing
library("rpart")
library("rpart.plot")
library("caret")
library("e1071")

# Calculating first classification tree model for minimum split 200
tree_1<-rpart(success ~ ., 
              data = CDtrain, method = "class", control = rpart.control(minsplit = 200))
rpart.plot(tree_1,type=1, extra = 2,under = TRUE)
title(sub = "Classification Tree with minsplit = 200")

# Calculating second classification tree model for minimum split 100
tree_2<-rpart(success ~ ., 
              data = CDtrain, method = "class", control = rpart.control(minsplit = 100))
rpart.plot(tree_2,type=1, extra = 2,under = TRUE)
title(sub = "Classification Tree with minsplit = 100")

# Calculating third classification tree model for minimum split 50
tree_3<-rpart(success ~ ., 
              data = CDtrain, method = "class", control = rpart.control(minsplit = 50))
rpart.plot(tree_3,type=1, extra = 2,under = TRUE)
title(sub = "Classification Tree with minsplit = 50")

# Predicting values for regression tree_1
pred1<- predict(tree_1, CDtest, type="class")

# Running confusion matrix for regression tree_1
confusionMatrix(pred1, CDtest$success, dnn = c("Predicted","Actual"))

# Predicting values for regression tree_2
pred2<- predict(tree_2, CDtest, type="class")

# Running confusion matrix for regression tree_2
confusionMatrix(pred2, CDtest$success, dnn = c("Predicted","Actual"))

# Predicting values for regression tree_2
pred3<- predict(tree_3, CDtest, type="class")

# Running confusion matrix for regression tree_3
confusionMatrix(pred3, CDtest$success, dnn = c("Predicted","Actual"))

# Tree's 2 and 3 both have the same accuracy and statistics and
# therefore we would go with them
