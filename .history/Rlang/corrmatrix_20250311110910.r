# Load necessary libraries
library(ggplot2)
library(reshape2)

# Read the dataset
df <- read.csv("dataset/cleaned_data.csv")

# Select only numerical columns
num_cols <- sapply(df, is.numeric)
df_num <- df[, num_cols]

# Compute the correlation matrix
cor_matrix <- cor(df_num, use = "pairwise.complete.obs")

# Convert the correlation matrix to a tidy format
cor_data <- melt(cor_matrix)

# Create a correlation heatmap using ggplot2 with a box grid
p <- ggplot(cor_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black", ;i = 0.5) +  # Add black grid lines
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid.major = element_blank(),  # Remove default grid lines
        panel.grid.minor = element_blank())  # Remove minor grid lines

# Save the plot as a PDF
ggsave("correlation_matrix.pdf", plot = p, width = 10, height = 8)
