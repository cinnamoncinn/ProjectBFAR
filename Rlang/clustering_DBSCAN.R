# Load required libraries
library(ggplot2)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(cluster)
library(dbscan)  # Added for DBSCAN

# Load the dataset
df <- read.csv("dataset/cleaned_data.csv")

# Select only numeric columns for PCA
df_numeric <- df %>% select(where(is.numeric))

# Replace infinite values with NA (if any exist)
df_numeric[df_numeric == Inf | df_numeric == -Inf] <- NA

# Standardize the data
df_numeric <- na.omit(df_numeric)

# Perform PCA
pca_result <- prcomp(df_numeric, center = TRUE, scale. = TRUE)

# Compute PCA scores
pca_scores <- as.data.frame(pca_result$x[, 1:2])
colnames(pca_scores) <- c("PC1", "PC2")

# ---- K-Means Clustering (Existing) ----
optimal_k <- 3  # Adjust based on Elbow plot result
kmeans_result <- kmeans(pca_scores, centers = optimal_k, nstart = 10)

# Add cluster assignments to PCA scores
pca_scores$kmeans_cluster <- as.factor(kmeans_result$cluster)

# ---- DBSCAN Clustering ----
eps_value <- 0.5  # Adjust based on data distribution
min_pts <- 5  # Minimum points to form a cluster
dbscan_result <- dbscan(pca_scores[, 1:2], eps = eps_value, minPts = min_pts)

# Add DBSCAN cluster labels to PCA scores
pca_scores$dbscan_cluster <- as.factor(dbscan_result$cluster)  # -1 represents noise

# ---- PCA Cluster Plot with DBSCAN ----
ggplot(pca_scores, aes(x = PC1, y = PC2, color = dbscan_cluster)) +
  geom_point(alpha = 0.8, size = 3) +
  scale_color_manual(values = c("red", "blue", "green", "purple", "black")) + 
  theme_minimal() +
  labs(title = "DBSCAN Clustering on PCA-Reduced Data",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "DBSCAN Cluster") +
  theme(legend.position = "right")
