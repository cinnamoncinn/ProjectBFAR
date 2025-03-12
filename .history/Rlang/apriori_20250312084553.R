# Load necessary libraries
library(arules)
library(arulesViz)
library(readxl)

# Read the dataset from Excel
df <- read_excel("BFAR-DATASET2.xlsx", sheet = "FINAL_DATASET")

# Function to categorize numerical values into "Low", "Medium", "High"
categorize <- function(x) {
  quantiles <- quantile(x, probs = c(0.33, 0.66), na.rm = TRUE, type = 7)
  
  if (length(unique(quantiles)) < 2) {
    return(rep("Medium", length(x))) 
  }
  
  cut(x, breaks = c(-Inf, unique(quantiles), Inf), 
      labels = c("Low", "Medium", "High"), include.lowest = TRUE)
}

# Apply categorization only to numeric columns
df_cat <- as.data.frame(lapply(df, function(col) {
  if (is.numeric(col)) categorize(col) else as.factor(col)
}))

# Convert dataframe into transactions format
df_trans <- as(df_cat, "transactions")

# Reduce memory usage by increasing support and limiting rule length
rules <- apriori(df_trans, 
                 parameter = list(supp = 0.2, conf = 0.5, minlen = 2, maxlen = 3))

# Visualize the association rules
plot(rules, method = "graph", control = list(max = 50))
