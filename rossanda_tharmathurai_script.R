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

ggplot(crowd_data,aes(x = num_perks, y = pct_raised.log)) + geom_point(color = "blue") + geom_smooth(method = "lm", color = "red") + scale_y_continuous(labels=scales::percent, limits = c(0,3)) + xlab("Number of Perks") + ylab("Percentage Raised") + ggtitle("Percentage Raised vs. Number of Perks") + theme(plot.title = element_text(hjust = 0.5))
