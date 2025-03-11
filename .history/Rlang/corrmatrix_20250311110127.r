# Load necessary libraries
library(ggplot2)
library(corrplot)
library(reshape2)
# Read the dataset
df <- read.csv("dataset/cleaned_data.csv")

# Select only numerical columns
num_cols <- sapply(df, is.numeric)
df_num <- df[, num_cols]

# Compute the correlation matrix
cor_matrix <- cor(df_num, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix)

# Visualize the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.6, tl.col = "black")
