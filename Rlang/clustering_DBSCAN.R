# Load required libraries
library(arules)
library(arulesViz)

# Load the dataset (replace 'rules.csv' with your file)
rules_data <- read.csv("C:/Users/CLSD_User/Documents/GitHub/ProjectBFAR/rules.csv")

# Convert necessary columns to the required format
rules_data$confidence <- as.numeric(rules_data$confidence)  # Ensure confidence is numeric

# Filter rules with confidence ≥ 80%
filtered_rules <- subset(rules_data, confidence >= 0.80)

# Print the filtered rules as a table
print(filtered_rules)

# (Optional) Save the filtered rules to a new CSV file
write.csv(filtered_rules, "filtered_rules.csv", row.names = FALSE)

