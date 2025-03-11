# Load required libraries
library(arules)
library(arulesViz)
library(dplyr)
# Load dataset
dataset <- read.csv("dataset/cleaned_data.csv", stringsAsFactors = TRUE)

dataset[is.na(dataset)] <- "Missing"
dataset[] <- lapply(dataset, function(x) if (is.integer(x)) as.character(x) else x)
dataset <- dataset %>%
  mutate_if(is.integer, as.character)
dataset <- as.data.frame(dataset)  # Ensure it's a proper dataframe
which(sapply(dataset, is.list))

dataset[] <- lapply(dataset, function(x) {
  if (is.list(x)) {
    as.character(sapply(x, paste, collapse = " "))  # Flatten lists to strings
  } else if (is.integer(x)) {
    as.character(x)  # Convert integers to strings
  } else {
    x
  }
})
empty_cols <- which(sapply(dataset, function(x) length(x) == 0))
dataset <- dataset[, -empty_cols, drop = FALSE]  # Remove them

# Select categorical columns for analysis
categorical_cols <- c("B5.SEX", "B6.M.STATUS", "B7.EDUCATION", "B8.Household.Size")
dataset_subset <- dataset[, categorical_cols]

missing_cols <- setdiff(categorical_cols, colnames(dataset))
print(missing_cols)

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
