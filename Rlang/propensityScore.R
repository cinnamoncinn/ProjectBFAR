#install.packages(c('ggplot2', 'readxl', 'dplyr', 'caret', 'cluster', 'factoextra', 'randomForest', 'corrplot', 'igraph', 'ggcorrplot', 'rpart', 'rpart.plot'))
library(ggplot2)
library(readxl)
library(dplyr)
library(caret)
library(cluster)
library(factoextra)
library(randomForest)
library(corrplot)
library(igraph)
library(ggcorrplot)
library(rpart)
library(rpart.plot)

# Load the dataset
data <- read_excel("Dataset/BFAR-DATA-2.xlsx", sheet = "Sheet1")

# Convert categorical variables to factors
data$REGION <- as.factor(data$REGION)
data$GROUP <- as.factor(data$GROUP)
data <- na.omit(data)  # Removes rows with NA

# Create a folder to save plots
output_dir <- "plots"
dir.create(output_dir, showWarnings = FALSE)

# Function to apply white background theme
theme_white <- theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA))

# Histograms for Data Distribution
hist_plot <- ggplot(data, aes(x = P_SCORE)) +
  geom_histogram(binwidth = 0.05, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of P_SCORE", x = "P_SCORE", y = "Count") +
  theme_white
ggsave(filename = file.path(output_dir, "histogram_p_score.png"), plot = hist_plot)

# Boxplot of P_SCORE by REGION
boxplot_plot <- ggplot(data, aes(x = REGION, y = P_SCORE, fill = REGION)) +
  geom_boxplot() +
  labs(title = "P_SCORE by REGION", x = "Region", y = "P_SCORE") +
  theme_white
ggsave(filename = file.path(output_dir, "boxplot_p_score_region.png"), plot = boxplot_plot)

# Scatter plots for correlations
scatter_plot <- ggplot(data, aes(x = `C01 - INCOME`, y = P_SCORE)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Income vs P_SCORE", x = "Income", y = "P_SCORE") +
  theme_white
ggsave(filename = file.path(output_dir, "scatter_income_p_score.png"), plot = scatter_plot)

# Heatmap for density visualization
numeric_data <- select_if(data, is.numeric)
corr_matrix <- cor(numeric_data, use = "complete.obs")
png(filename = file.path(output_dir, "heatmap_density.png"), width = 800, height = 600)
ggcorrplot(corr_matrix, method = "square", type = "full", lab = TRUE)
dev.off()

# Correlation Matrix
png(filename = file.path(output_dir, "correlation_matrix.png"), width = 800, height = 600)
corrplot(corr_matrix, method = "color", tl.cex = 0.7)
dev.off()

# Clustering (K-Means)
scaled_data <- scale(numeric_data)
kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)
data$Cluster <- as.factor(kmeans_result$cluster)

cluster_plot <- ggplot(data, aes(x = `C01 - INCOME`, y = P_SCORE, color = Cluster)) +
  geom_point(alpha = 0.7) +
  labs(title = "K-Means Clustering of P_SCORE vs Income", x = "Income", y = "P_SCORE") +
  theme_white
ggsave(filename = file.path(output_dir, "kmeans_clustering.png"), plot = cluster_plot)

# Decision Tree Model
set.seed(123)
trainIndex <- createDataPartition(data$P_SCORE, p = 0.7, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

dt_model <- rpart(P_SCORE ~ ., data = train_data, method = "anova")

# Save decision tree plot
png(filename = file.path(output_dir, "decision_tree.png"), width = 800, height = 600)
rpart.plot(dt_model, main = "Decision Tree for P_SCORE")
dev.off()

# Model Evaluation
pred <- predict(dt_model, test_data)
RMSE <- sqrt(mean((pred - test_data$P_SCORE)^2))
print(paste("Root Mean Square Error:", RMSE))

# Network Graph for Relationships
network_data <- data.frame(from = sample(unique(data$REGION), 50, replace = TRUE),
                           to = sample(unique(data$GROUP), 50, replace = TRUE))
graph <- graph_from_data_frame(network_data)
png(filename = file.path(output_dir, "network_graph.png"), width = 800, height = 600)
plot(graph, vertex.size = 10, vertex.label.cex = 0.7, main = "Network Graph of Regions and Groups")
dev.off()

# Decision Tree for Impact Prediction
set.seed(123)
dt_model <- rpart(GROUP ~ ., data = data, method = "class")  
rpart.plot(dt_model)
ggsave("plots/decision_tree.png", width = 8, height = 5)

# Pairwise Scatter Plot for Correlations
pairwise_plot <- ggpairs(numeric_data)
ggsave(filename = file.path(output_dir, "pairwise_scatter_plot.png"), plot = pairwise_plot)
