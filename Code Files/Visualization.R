# Install packages if not already installed
install.packages(c("dplyr", "ggplot2", "caret", "randomForest", "corrplot"))

# Load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(corrplot)

#Load Data
churn <- read.csv("F:/BDA/2nd year/1st sem/Group Project/emp_cleaned2.csv")

str(churn)

#converting catogorical variable to numerical - identifing
churn$Attrition <- as.factor(churn$Attrition)
numeric_data <- churn %>% select_if(is.numeric)
corrplot(cor(numeric_data), method = "circle", type = "upper", tl.cex = 0.7)

#Attrition by monthly income
ggplot(churn, aes(x = Attrition, y = MonthlyIncome, fill = Attrition)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Attrition by Monthly Income", y = "Monthly Income", x = "Attrition")

#Attrition by OT
ggplot(churn, aes(x = OverTime, fill = Attrition)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Attrition by Overtime", y = "Proportion", x = "OverTime")

#Attrition by jOB satisfaction
ggplot(churn, aes(x = JobSatisfaction, fill = Attrition)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Attrition by Job Satisfaction", y = "Proportion", x = "Job Satisfaction")




ggplot(churn[churn$IncomeCategory == "Low Income", ], aes(x = Age, fill = Attrition)) +
  geom_bar(position = "fill") +
  labs(title = "Attrition by Marital Status (Low Income Employees)", x = "Marital Status", y = "Proportion")











