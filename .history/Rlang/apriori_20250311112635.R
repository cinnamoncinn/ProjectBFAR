# Load necessary libraries
library(arules)
library(arulesViz)

# Read the dataset
df <- read.csv("dataset/cleaned_data.csv")

# Convert numerical data into categorical bins
# Example: Convert values into "High", "Medium", and "Low"
categorize <- function(x) {
  quantiles <- quantile(x, probs = c(0.33, 0.66), na.rm = TRUE)
  cut(x, breaks = c(-Inf, quantiles, Inf), labels = c("Low", "Medium", "High"))
}

df_cat <- as.data.frame(lapply(df, function(col) {
  if (is.numeric(col)) categorize(col) else as.factor(col)
}))

# Convert dataframe into transactions format
df_trans <- as(df_cat, "transactions")

# Apply Apriori Algorithm
rules <- apriori(df_trans, 
                 parameter = list(supp = 0.1, conf = 0.6, minlen = 2))

# Inspect top rules
inspect(head(sort(rules, by="confidence"), 10))

# Visualize the association rules
plot(rules, method = "graph", control = list(type = "items"))
