library(ggplot2)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(cluster)

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

# ---- Elbow Method for Optimal Clusters ----
wss <- function(k) {
  kmeans(pca_scores, centers = k, nstart = 10, iter.max = 100)$tot.withinss
}

# Compute WSS for k = 1 to 10
k.values <- 1:10
wss_values <- sapply(k.values, wss)

# Plot Elbow Method
elbow_plot <- ggplot(data.frame(k = k.values, wss = wss_values), aes(x = k, y = wss)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title = "Elbow Method for Optimal Clusters",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares")

print(elbow_plot)

# ---- K-Means Clustering ----
optimal_k <- 1 # 3 def Adjust based on Elbow plot result
kmeans_result <- kmeans(pca_scores, centers = optimal_k, nstart = 10) #nstart 10

# Add cluster assignments to PCA scores
pca_scores$cluster <- as.factor(kmeans_result$cluster)

# ---- Outlier Detection ----
mahal_dist <- mahalanobis(pca_scores[, 1:2], colMeans(pca_scores[, 1:2]), cov(pca_scores[, 1:2]))
threshold <- quantile(mahal_dist, 0.98, na.rm = TRUE) #0.975
pca_scores$outlier <- ifelse(mahal_dist > threshold, "Outlier", "Normal")

# ---- PCA Cluster Plot with Outliers ----
ggplot(pca_scores, aes(x = PC1, y = PC2, color = cluster, shape = outlier)) +
  geom_point(alpha = 0.8, size = 3) +
  scale_color_manual(values = c("1" = "blue", "2" = "red", "3" = "green")) +
  scale_shape_manual(values = c("Normal" = 16, "Outlier" = 8)) +
  theme_minimal() +
  labs(title = "2D PCA Cluster Plot with Outlier Detection",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster",
       shape = "Outlier Status") +
  theme(legend.position = "right")