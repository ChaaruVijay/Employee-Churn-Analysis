library(caret)
library(randomForest)
library(pROC)
library(dplyr)


data <- read.csv("F:/BDA/2nd year/1st sem/Group Project/dump excel/emp_cleaned2 - Original.csv")
data

#Convertion
data$BusinessTravel <- as.factor(emp$BusinessTravel)
data$Gender <- as.factor(emp$Gender)
data$JobRole <- as.factor(emp$JobRole)
data$MaritalStatus <- as.factor(emp$MaritalStatus)
data$OverTime <- as.factor(emp$OverTime)
data$EnvironmentSatisfaction <- as.factor(emp$EnvironmentSatisfaction)
data$JobInvolvement <- as.factor(emp$JobInvolvement)
data$JobSatisfaction <- as.factor(emp$JobSatisfaction)
data$PerformanceRating <- as.factor(emp$PerformanceRating)
data$RelationshipSatisfaction <- as.factor(emp$RelationshipSatisfaction)
data$WorkLifeBalance <- as.factor(emp$WorkLifeBalance)
data$Attrition <- ifelse(test = emp$Attrition == 'Yes', yes = 'Resigned', no = 'Not Resigned')
data$Attrition <- as.factor(emp$Attrition)




# For reproducibility
set.seed(123)  
trainIndex <- createDataPartition(data$Attrition, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]


# Train logistic regression model
logistic_model <- glm(Attrition ~ ., data = trainData, family = "binomial")
summary(logistic_model)

if(!require(car)) install.packages("car", dependencies = TRUE)
library(car)
# Check for multicollinearity
vif(logistic_model)  


ll.null <- logistic_model$null.deviance / -2
ll.proposed <- logistic_model$deviance / -2
pseudo_r2 <- (ll.null - ll.proposed) / ll.null  # Pseudo R²
p_value <- 1 - pchisq(2 * (ll.proposed - ll.null), df = (length(logistic_model$coefficients) - 1))

print(paste("Pseudo R²: ", round(pseudo_r2, 4)))
print(paste("p-value: ", round(p_value, 4)))


if(!require(caret)) install.packages("caret", dependencies = TRUE)
library(caret)

# Predict probabilities and classes
predicted_prob <- predict(logistic_model, testData, type = "response")
predicted_class <- ifelse(predicted_prob > 0.5, "Yes", "No")
predicted_class <- factor(predicted_class, levels = c("No", "Yes"))

# Confusion Matrix
confusion_matrix <- confusionMatrix(predicted_class, testData$Attrition)
print(confusion_matrix)



#Model Discrimination

#predicted probabilities
predicted_prob <- predict(logistic_model, testData, type = "response")

#Create a data frame for visualization
predicted_data <- data.frame(
  probability_of_attrition = predicted_prob,
  Attrition = testData$Attrition
)

# Sort data by predicted probabilities (from low to high)
predicted_data <- predicted_data[order(predicted_data$probability_of_attrition),]
predicted_data$rank <- 1:nrow(predicted_data)

# Step 3: Visualize the predicted probabilities using ggplot2
library(ggplot2)

ggplot(data = predicted_data, aes(x = rank, y = probability_of_attrition)) +
  geom_point(aes(color = Attrition), alpha = 1, shape = 4, stroke = 2) +
  xlab("Index (Rank)") +
  ylab("Predicted Probability of Attrition") +
  theme_minimal() +
  ggtitle("Model Discrimination: Predicted Probability of Attrition") +
  scale_color_manual(values = c("blue", "red"))
