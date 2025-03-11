# Load required libraries
library(arules)
library(arulesViz)

# Load dataset
dataset <- read.csv("dataset/cleaned_data.csv", stringsAsFactors = TRUE)
colnames(dataset)

# Select categorical columns for analysis
categorical_cols <- c("B5:SEX", "B6:M-STATUS", "B7:EDUCATION", 
                      "C1:TOT_INCOME/A", "C2:INCOME/B/FISH", "B8:Household Size")

# Subset the dataset to only include categorical attributes
dataset_subset <- dataset[categorical_cols]

# Convert the data frame to a list of transactions
dataset_list <- split(dataset_subset, seq(nrow(dataset_subset)))

# Convert to transactions object
transactions <- as(dataset_list, "transactions")

# Apply the Apriori algorithm
rules <- apriori(transactions, 
                 parameter = list(supp = 0.01, conf = 0.5, minlen = 2))

# View top association rules
inspect(head(rules, 10))

# Visualize the rules
plot(rules, method="graph", engine="htmlwidget")
