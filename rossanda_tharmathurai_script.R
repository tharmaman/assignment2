# Importing the reduced crowd fund data
crowd_data <- read.csv("~/Documents/U3 Classes/Business Intel & Data Analytics/Assignments/Assignment 2/crowd_data_new.csv", head = TRUE)
 
# Initiating ggplot2 
install.packages("ggplot2")
library("ggplot2")

# dependent variables: 
# pct_raised (continuous variable) 
# and success (dichotomous variable).
  
# predictor variables:
# category, city, state, goal, fb_total_likes,
# total_visitors, start_month, end_month, prior campaigns, 
# number of perks

# QUESTION 1

#Using a histogram for the continuous variable (pct_raised)
ggplot(crowd_data,aes(pct_raised)) + geom_histogram(binwidth = .05, fill="#FF9999", colour="black") + xlim(0, 2) + xlab("Percentage Raised") + ylab("Frequency") + ggtitle("Percentage Raised Histogram") + theme(plot.title = element_text(hjust = 0.5))

#Using a bar plot for the dichotomous variable (success)

#Reformatting to show percentages & yes/no instead of count and 0/1
crowd_data2 <- crowd_data

crowd_data2$success2 <- ifelse(crowd_data2$success==1, "Yes", "No")
crowd_data2$percentage_success <- crowd_data2$success / sum(crowd_data2$success)

ggplot(crowd_data2, aes(x = success2)) + geom_bar(aes (y = (..count..)/sum(..count..)), fill="#890C3C", colour="black") + scale_y_continuous(labels=scales::percent) + xlab("Success?") + ylab("Percentage") + ggtitle("Success Proportions") + theme(plot.title = element_text(hjust = 0.5))

#Transforming pct_raised with the log function
pct_raised.log = log(crowd_data$pct_raised)
crowd_data$pct_raised.log = pct_raised.log

# Plotting the Logged Percentage Raised
ggplot(crowd_data,aes(pct_raised.log)) + geom_histogram(binwidth = .05, fill="#FF9999", colour="black") + xlim(0, 2) + xlab("Percentage Raised") + ylab("Frequency") + ggtitle("Percentage Raised Logged Histogram") + theme(plot.title = element_text(hjust = 0.5))

#Categorical Variables

#category predictor
factor_category_vector = as.factor(crowd_data$category)

#append category predictor to data
crowd_data = data.frame(crowd_data, factor_category_vector)
crowd_data$factor_category_vector = as.numeric(factor_category_vector)

#city predictor
factor_city_vector = as.factor(crowd_data$city)

#city category predictor to data
crowd_data = data.frame(crowd_data, factor_city_vector)
crowd_data$factor_city_vector = as.numeric(factor_city_vector)

#city predictor
factor_state_vector = as.factor(crowd_data$state)

#city category predictor to data
crowd_data = data.frame(crowd_data, factor_state_vector)
crowd_data$factor_state_vector = as.numeric(factor_state_vector)

#Scatter plot: average_pct_raised vs. number of perks

ggplot(crowd_data,aes(x = num_perks, y = pct_raised.log)) + geom_point(color = "blue") + geom_smooth(method = "lm", color = "red") + scale_y_continuous(labels=scales::percent, limits = c(0,3)) + xlim(0,30) + xlab("Number of Perks") + ylab("Percentage Raised") + ggtitle("Percentage Raised vs. Number of Perks") + theme(plot.title = element_text(hjust = 0.5))

# QUESTION 2

# Prepping data for MLR
# dependent variables: 
# pct_raised (continuous variable) 

# predictor variables:
# category, city, state, goal, fb_total_likes,
# total_visitors, start_month, end_month, prior campaigns, 
# number of perks

# Creating new data frame for MLR analysis

crowd_data_mlr = crowd_data[,c(19,20,21,22,6,8,12,13,15,17,18)]

# Splitting data set
library(caTools)
set.seed(123)
sample=sample.split(crowd_data_mlr, SplitRatio = .60)
crowd_data_mlr.train<-subset(crowd_data_mlr, sample == TRUE)
crowd_data_mlr.test<-subset(crowd_data_mlr, sample == FALSE)
rownames(crowd_data_mlr.train)<-1:7328
rownames(crowd_data_mlr.test)<-1:6106

# Running correlation for all variables
cor(crowd_data_mlr)

# Running first regression for all variables
reg1 = lm(pct_raised.log ~., data = crowd_data_mlr.train)
summary(reg1)

# Initiating outlier test package

install.packages("car")
library("car")

# Checking for variables with multicolinearity (>5)
vif(reg1)

# Eliminating fb_total_likes (7.622236) & unique_visitors (7.623186)
crowd_data_mlr.train = crowd_data_mlr.train[,-c(6,7)]

# Checking for outliers
outlierTest(reg1)

# Removing outliers (1286, 7075, 6148, 5782)
crowd_data_mlr = crowd_data_mlr[-c(1286,7075,6148,5782),]

# Running second regression after removing outliers and multicolinear variables
reg2 = lm(pct_raised.log ~., data = crowd_data_mlr.train)
summary(reg2) 

# Removing insignificant predictors (factor_city_vector, start_month)
crowd_data_mlr.train = crowd_data_mlr.train[,-c(3,6)]

# Running third regression after removing insignificant predictors
reg3 = lm(pct_raised.log ~., data = crowd_data_mlr.train)
summary(reg3) 

# Using reg3 model to predict validation data
test1 <- predict(reg3, crowd_data_mlr.test)
summary(test1)

# Checking reg3 model performance with RMSE
rmse.test1 <- sqrt(mean((test1 - crowd_data_mlr.test$pct_raised.log)^2, na.rm = TRUE))
summary(rmse.test1)

# Using reg2 model to predict validation data
test2 <- predict(reg2, crowd_data_mlr.test)
summary(test2)

# Checking reg2 model performance with RMSE
rmse.test2 <- sqrt(mean((test2 - crowd_data_mlr.test$pct_raised.log)^2, na.rm = TRUE))
summary(rmse.test2)

# Comparing test1 with test2
rmse.test1
summary(test1)
rmse.test2
summary(test2)

# Test1 using reg3 has a smaller RMSE than test2 using reg2
# Therefore, reg3 is the better model at predicting


# QUESTION 3

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
