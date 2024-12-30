# Clear Global environment
rm(list=ls())

# load the library
library(dplyr)
library(ggplot2)
library(ModelMetrics)


cab_data <- read.csv("C:/Users/HP/Desktop/BPB/datasets/cab_data.csv")

cab_data <- read.csv("cab_data.csv")

# Visualize the linear relationship between distance and time
ggplot(cab_data, aes(x = Distance, y = Time_Taken)) +
  geom_point(col = "steelblue4") +
  labs(title = "Linear Relationship between Distance and Time",
       x = "Distance (kms)",
       y = "Time Taken (minutes)") +
  theme_bw()


# Split data into train and test
set.seed(123)

rows=seq(1,nrow(cab_data),1)

trainRows=sample(rows,(80*nrow(cab_data))/100)

train = cab_data[trainRows,] 
test = cab_data[-trainRows,]

lm_time <- lm(Time_Taken ~ Distance, data = train)

#Summary of the model
summary(lm_time)

#predict on test
predicted_values <- predict(lm_time, newdata=test)

#Create a dataframe with Actuals and Predicted values
compare_df <- data.frame(Actual = test$Time_Taken, Predicted = predicted_values)

#Root Mean Squared Error (RMSE)
rmse(compare_df$Actual,compare_df$Predicted)

#Mean Absolute Error (MAE)
mae(compare_df$Actual,compare_df$Predicted)

loan_df <- read.csv("C:/Users/HP/Desktop/BPB/datasets/loan_df.csv")

loan_df <- read.csv("loan_df.csv")

loan_df$default <- as.factor(loan_df$default)

# Distribution of the Default
ggplot(loan_df, aes(x = credit_score, fill = default)) +
  geom_histogram(binwidth = 100, colour = "black") +
  labs(title = "Default vs. Credit Score",
       x = "Credit Score",
       y = "Frequency") +
  theme_bw() 

# Split data into train and test
set.seed(123)

rows=seq(1,nrow(loan_df),1)

trainRows=sample(rows,(80*nrow(loan_df))/100)

train = loan_df[trainRows,] 
test = loan_df[-trainRows,]

# Train a logistic regression model
LogReg <- glm(default ~ credit_score + debt_to_income_ratio + age, 
              data=train, family=binomial)

# Summary of the model
summary(LogReg)


# Predict the Model on Test Set
fitted_results <- predict(LogReg,test,type='response')

fitted_class <- ifelse(fitted_results > 0.5,1,0)

# Confusion Matrix
conf_matrix = table(test$default,fitted_class)
conf_matrix

# Calculate True Positives (TP), True Negatives (TN), False Positives (FP), and False Negatives (FN)
TP <- conf_matrix[2,2]
TN <- conf_matrix[1,1]
FP <- conf_matrix[1,2]
FN <- conf_matrix[2,1]


# Calculate Accuracy
accuracy <- (TP + TN) / sum(conf_matrix)
accuracy
	
# Calculate Precision
precision <- TP / (TP + FP)
precision

# Calculate Recall (Sensitivity)
recall <- TP / (TP + FN)
recall

# load the required libraries
library(rpart)
library(rpart.plot)

# Train a Decision Tree model 
Model_rpart= rpart(default~ credit_score + debt_to_income_ratio,
                   data=train, method="class",
                   maxdepth = 5)

# Visualise the decision tree
rpart.plot(Model_rpart)

# Decision tree rules
rpart.rules(Model_rpart, cover = T)

#Predicting on Test
pred_rpart <- predict(Model_rpart,test,type="class")

# Confusion Matrix
conf_matrix = table(test$default,pred_rpart)
conf_matrix


# Calculate True Positives (TP), True Negatives (TN), False Positives (FP), and False Negatives (FN)
TP <- conf_matrix[2,2]
TN <- conf_matrix[1,1]
FP <- conf_matrix[1,2]
FN <- conf_matrix[2,1]

# Calculate Accuracy
accuracy <- (TP + TN) / sum(conf_matrix)
accuracy

# Calculate Precision
precision <- TP / (TP + FP)
precision

# Calculate Recall (Sensitivity)
recall <- TP / (TP + FN)
recall

