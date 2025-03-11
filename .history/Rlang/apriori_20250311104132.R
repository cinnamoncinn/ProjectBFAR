# Load necessary libraries
library(arules)
library(arulesViz)
library(dplyr)

# Load dataset
data <- read.csv("dataset/cleaned_data.csv", stringsAsFactors = TRUE)

# Convert numeric columns to categorical (example: income levels)
data$C1.TOT_INCOME.A <- cut(data$C1.TOT_INCOME.A, 
                            breaks=c(-Inf, 5000, 10000, 20000, Inf), 
                            labels=c("Low", "Medium", "High", "Very High"))

# Select relevant categorical columns for transaction analysis
selected_columns <- c("A1.AREA", "A2.GROUP", "B5.SEX", "B7.EDUCATION", "C1.TOT_INCOME.A")
transactions <- data[selected_columns]

# Convert to transactions format
transactions <- as(transactions, "transactions")

# Apply Apriori algorithm
rules <- apriori(transactions, parameter = list(supp = 0.05, conf = 0.6))

# Visualize the rules
plot(rules, method="graph", engine="igraph")