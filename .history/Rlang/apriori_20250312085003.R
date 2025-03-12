# Load necessary libraries
library(arules)
library(arulesViz)

# Read the dataset
df <- read.csv("dataset/cleaned_data.csv")

# Function to categorize numerical values into "Low", "Medium", "High"
categorize <- function(x) {
  # Compute 33% and 66% quantiles safely
  quantiles <- quantile(x, probs = c(0.33, 0.66), na.rm = TRUE, type = 7)
  
  # Ensure quantiles are unique, otherwise adjust manually
  if (length(unique(quantiles)) < 2) {
    return(rep("Medium", length(x)))  # Assign "Medium" if no proper bins exist
  }
  
  # Apply cut() with exactly 3 intervals
  cut(x, breaks = c(-Inf, unique(quantiles), Inf), 
      labels = c("Low", "Medium", "High"), include.lowest = TRUE)
}

# Apply categorization only to numeric columns
df_cat <- as.data.frame(lapply(df, function(col) {
  if (is.numeric(col)) categorize(col) else as.factor(col)
}))

# Convert dataframe into transactions format
df_trans <- as(df_cat, "transactions")

# Apply Apriori Algorithm
rules <- apriori(df_trans, 
                 parameter = list(supp = 0.2, conf = 0.5, minlen = 2, maxlen = 4z))

# Visualize the association rules
plot(rules, method = "graph", control = list(max = 150), engine = "ggplot2")
