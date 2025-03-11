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

cor_data <- melt(cor_matrix)
# Print the correlation matrix
print(cor_matrix)

# Visualize the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.6, tl.col = "black")

# Create a correlation heatmap using ggplot2
p <- ggplot(cor_data, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Save the plot as a PDF
ggsave("correlation_matrix.pdf", plot = p, width = 10, height = 8)