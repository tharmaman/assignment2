# Importing the crowd fund data
crowd_data <- read.csv("~/Documents/U3 Classes/Business Intel & Data Analytics/Assignments/Assignment 2/crowd_data.csv", head = TRUE)
 
# Initiating ggplot2 
install.packages("ggplot2")
library("ggplot2")

# dependent variables: 
# pct_raised (continuous variable) 
# and success (dichotomous variable).

# predictor variables:
# category, city, state, goal, fb_total_likes,
# duration, start_month, success_month, prior campaigns, 
# number of perks

#Using a histogram for the continuous variable (pct_raised)
hist(crowd_data$pct_raised)

#Using a bar plot for the continuous variable (success)
ggplot(data = crowd_data) + geom_bar(aes(success)) + labs(x = "success", y = "Frequency")
