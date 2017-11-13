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
ggplot(crowd_data,aes(pct_raised)) + geom_histogram(binwidth = .05, fill="#FF9999", colour="black") + xlim(0, 2) + xlab("Percentage Raised") + ylab("Frequency") + ggtitle("Percentage Raised Histogram") + theme(plot.title = element_text(hjust = 0.5))

#Using a bar plot for the discrete variable (success)
ggplot(crowd_data, aes(x = success)) + geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(formatter = 'percent')



