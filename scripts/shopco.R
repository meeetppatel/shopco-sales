################################################################################
#                            CLASS PROJECT
################################################################################

# This is the class project for the MGT585 course (15 points, individual assignment)

# Please write your name here-

# This script is based on the Shopco data- Shopco is a mall operator
# Use the Shopco caselet and data (consumer.csv,  purchase.csv) 
# on D2L to answer questions below
# Think about the managerial implications as you go along
# You will use the knowledge from MGT585 that you have learned so far

################################################################################
#                                  MGT585
################################################################################

# We have covered following:

# Basic functions in R
# DPLYR
# Summary stats
# Viz. using GGPLOT
# Regression
# Classification
# Clustering (Session 8)

################################################################################
#                                    Data
################################################################################

library(dplyr)
library(ggplot2)

# Q1) Read the data in: consumer and purchase tables

# set your working directory and use read.csv() to read files
getwd()
setwd("../data")
consumer = read.csv("consumer.csv")
purchase = read.csv("purchase.csv")

################################################################################

# Q2) Let's first start with exploring various tables

# explore consumer table using 5 functions
# hint: loyalty status 1 is low and 2 is high
# age is in years
cat("\n--- Consumer Table Summary ---\n")
cat("\n1. Structure of the dataset:\n")
str(consumer)  # Structure of the dataset

cat("\n2. Summary statistics:\n")
summary(consumer)  # Summary of numerical variables

cat("\n3. Unique genders:\n")
unique(consumer$gender)  # Unique values in gender

cat("\n4. Loyalty Status Distribution:\n")
table(consumer$loyalty_status)  # Count of each loyalty status

cat("\n5. Age distribution:\n")
hist(consumer$age, main="Age Distribution of Consumers", xlab="Age", col="lightblue", border="black")


# explore purchase table using 5 functions
# Hint: real time message 1 is received and 0 is not received
# Sales are in $ dollars
cat("\n--- Purchase Table Summary ---\n")
cat("\n1. Structure of the dataset:\n")
str(purchase)  # Structure of the dataset

cat("\n2. Summary statistics:\n")
summary(purchase)  # Summary of numerical variables

cat("\n3. Real-time Message Distribution:\n")
table(purchase$realtime_message)  # Count of messages received vs. not received

cat("\n4. Sales Distribution Summary:\n")
summary(purchase$from_second_store_sales)  # Sales statistics

cat("\n5. Sales Histogram:\n")
hist(purchase$from_second_store_sales, main="Distribution of Sales from Second Store Onwards", 
     xlab="Sales ($)", col="lightgreen", border="black")



################################################################################
# Q3) correct the data types in consumer table #

# correct gender as a factor
consumer$gender <- as.factor(consumer$gender)

# correct loyalty_status as a factor
consumer$loyalty_status <- as.factor(consumer$loyalty_status)

# correct the data types in purchase table #

# correct realtime_message as a factor
purchase$realtime_message <- as.factor(purchase$realtime_message)


################################################################################
#                         Descriptive Analytics
################################################################################

# Q4) Consumer table analysis

# Continuous variables: Average age by gender
consumer %>% group_by(gender) %>% summarise(avg_age = mean(age, na.rm = TRUE))

# Categorical variables:
# Count of each gender
table(consumer$gender)

# Count of loyalty status
table(consumer$loyalty_status)

# Conclusion:
# Shopco's consumers are diverse in age and loyalty, and gender distribution appears balanced.

################################################################################
# Q5) Purchase table analysis

# Continuous variables: Average sales from second store onwards
summary(purchase$from_second_store_sales)

# Categorical variables: Count of real-time messages received
table(purchase$realtime_message)

# Conclusion:
# Real-time messages appear to influence consumer purchases; further analysis is needed.

################################################################################
# Q6) Combining consumer and purchase data

experiment <- inner_join(consumer, purchase, by="consumer_id")
nrow(experiment)  # Number of rows in experiment table

################################################################################
#                                  Predictive analytics
################################################################################
#                                    Regression
################################################################################

# Q7) Impact of age on sales

# Scatter plot
ggplot(experiment, aes(x=age, y=from_second_store_sales)) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method='lm', col='red') + 
  theme_minimal() + 
  labs(title="Impact of Age on Sales", x="Age", y="Sales from Second Store Onwards")

# Regression model
reg1 <- lm(from_second_store_sales ~ age, data=experiment)
summary(reg1)

################################################################################
# Q8) Impact of real-time mobile message on sales

# Bar chart
ggplot(experiment, aes(x=realtime_message, y=from_second_store_sales, fill=realtime_message)) + 
  geom_bar(stat="summary", fun="mean") + 
  theme_minimal() + 
  labs(title="Impact of Real-Time Messages on Sales", x="Real-Time Message", y="Average Sales")

# Regression model
reg2 <- lm(from_second_store_sales ~ realtime_message, data=experiment)
summary(reg2)

# Conclusion:
# Real-time messages significantly impact sales; managerial decision-making should consider targeted messaging.

################################################################################
#                                    Clustering
################################################################################

# Q9) Clustering customers based on age and sales

set.seed(123)  # For reproducibility
cluster_1 <- kmeans(experiment[, c("age","from_second_store_sales")], centers=3, nstart=20)

# Assign clusters
experiment$cluster <- as.factor(cluster_1$cluster)

# Visualization
ggplot(experiment, aes(x=age, y=from_second_store_sales, color=cluster)) + 
  geom_point(alpha=0.5) + 
  theme_minimal() + 
  labs(title="Customer Clusters Based on Age and Spending", x="Age", y="Sales from Second Store Onwards")

# Conclusion:
# Customers can be segmented into three distinct spending behaviors, which can guide marketing strategies.

################################################################################
