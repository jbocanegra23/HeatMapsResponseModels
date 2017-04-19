## This is a response modeling data set
## This data set can be found here: 
# https://community.watsonanalytics.com/wp-content/uploads/2015/03/WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv

library(dplyr)
library(readr)
install.packages("rminer")
install.packages("rattle")
library(rminer)
library(rattle)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

Customer_Data <- read_csv("WatsonSampleCustomerData.csv", col_names = TRUE)

colnames(Customer_Data) <- c("CustomerID", "State", "Customer_LT_Value", "Response", 
                             "Coverage", "Education", "Effective_Date", "Employment",
                             "Gender", "Income", "Location", "Marital_Status", "Monthly_Auto_Premium",
                             "Months_Last_Claim", "Months_Policy_Inception", "Open_Complaints",
                             "Policies", "Policy_Type", "Policy", "Renew_Offer", "Sales_Channel",
                             "Total_Claim_Amount", "Vehicle_Class", "Vehicle_Size")
Customer_Data <- Customer_Data[ ,c(-1, -2, -7, -18, -24)]

model <- rpart(Response ~ ., data = Customer_Data, method = "class")
model
summary(model)
prp(model)

# Given the classification model; the variables of importance are: 
# employment, renew offer, monthly auto premium, customer lifetime value and policy.
# however give the criteria, the best model is created using employment and renew offer
# Let's check this using a logistic regression model.

Customer_Data2 <- Customer_Data %>% select(Customer_LT_Value, 
                                           Response, 
                                           Employment, 
                                           Renew_Offer,
                                           Monthly_Auto_Premium,
                                           Policy)

# Transform the data into a format for regression

Customer_Data2$Response[Customer_Data2$Response == "Yes"] <- 1
Customer_Data2$Response[Customer_Data2$Response == "No"] <- 0

logistic <- lm(Response ~ ., data = Customer_Data2)
summary(logistic)

# Let's drop policy which appears to not be statistically significant; adj Rsq = 0.1478

Customer_Data3 <- Customer_Data2[, -c(1, 6)]
logistic2 <- lm(Response ~ ., data = Customer_Data3)
summary(logistic2)

# This verifies the decision tree data mining model that there is statistical significance
# Adj rsq = 0.1473. Without getting into validation this is an example of response modeling 
# If a customer responds favorably to an other depends on their employment status, their current monthly premium
# and the type of offer.

# Note: data mining techniques such as decision tree and clusters are VERY useful to find
# variables of importance. However, regression can the be used to verify the results and perhaps find
# addition variables of interest.

# Additional work could be done to model the favorable responses for each offer.


