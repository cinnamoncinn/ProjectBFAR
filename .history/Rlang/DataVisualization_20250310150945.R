# Download the ff packages
# install.packages(c('readxl', 'ggplot2', 'dplyr', 'tidyr', 'readr',"reshape2"))
# ----------------------------------------
# Load required libraries
# ----------------------------------------
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
# ----------------------------------------
# Define function to clean filenames (Windows-safe)
# ----------------------------------------
clean_filename <- function(filename) {
  gsub("[:?<>|*\"/\\\\]", "_", filename)  # Replace invalid characters with "_"
}

# ----------------------------------------
# Load dataset
# ----------------------------------------
df_A <- read_csv("/cleaned_data.csv", show_col_types = FALSE)

# Ensure 'A2:GROUP' exists and is a factor
if (!"A2:GROUP" %in% colnames(df_A)) {
  stop("Error: Column 'A2:GROUP' not found in dataset.")
}
df_A$`A2:GROUP` <- as.factor(df_A$`A2:GROUP`)

# ----------------------------------------
# Identify column types
# ----------------------------------------
categorical_cols <- names(df_A)[sapply(df_A, is.character) | sapply(df_A, is.factor)]
numerical_cols <- names(df_A)[sapply(df_A, is.numeric)]

# Remove 'A2:GROUP' from categorical list (since it's the grouping column)
categorical_cols <- setdiff(categorical_cols, "A2:GROUP")

# ----------------------------------------
# Create a directory for saving plots
# ----------------------------------------
if (!dir.exists("visualizations")) {
  dir.create("visualizations")
}
# ----------------------------------------
# Generate Stacked Bar Charts (Categorical Variables)
# ----------------------------------------
for (col in categorical_cols) {
  p <- ggplot(df_A, aes(x = `A2:GROUP`, fill = get(col))) + 
    geom_bar(position = "fill") +
    labs(title = paste("Distribution of", col, "by Group"), x = "Group", y = "Proportion") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white")) +
    theme(legend.background = element_rect(fill = "white", color = "black"))
  
  ggsave(filename = paste0("visualizations/stacked_bar", clean_filename(col), "_stacked_bar.png"),
         plot = p, width = 8, height = 5)
}

# ----------------------------------------
# Generate Non-Stacked Bar Charts (Categorical Variables)
# ----------------------------------------
for (col in categorical_cols) {
  p <- ggplot(df_A, aes(x = `A2:GROUP`, fill = get(col))) + 
    geom_bar(position = "dodge") +  # Non-stacked bar graph
    labs(title = paste("Count of", col, "by Group"), x = "Group", y = "Count") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white")) +
    theme(legend.background = element_rect(fill = "white", color = "black")) 
  
  ggsave(filename = paste0("visualizations/nonstacked_bar", clean_filename(col), "_non_stacked_bar.png"),
         plot = p, width = 8, height = 5)
}

# ----------------------------------------
# Generate Box Plots (Numerical Variables)
# ----------------------------------------
for (col in numerical_cols) {
  p <- ggplot(df_A, aes(x = `A2:GROUP`, y = get(col), fill = `A2:GROUP`)) +
    geom_boxplot() +
    labs(title = paste("Distribution of", col, "by Group"), x = "Group", y = col) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white")) +
    theme(legend.background = element_rect(fill = "white", color = "black"))
  
  ggsave(filename = paste0("visualizations/boxplots", clean_filename(col), "_boxplot.png"),
         plot = p, width = 8, height = 5)
}

# ----------------------------------------
# Generate Histograms (Numerical Variables)
# ----------------------------------------
for (col in numerical_cols) {
  p <- ggplot(df_A, aes(x = get(col), fill = `A2:GROUP`)) +
    geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
    facet_wrap(~`A2:GROUP`, scales = "free") +
    labs(title = paste("Histogram of", col, "by Group"), x = col, y = "Count") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white")) +
    theme(legend.background = element_rect(fill = "white", color = "black"))
  
  ggsave(filename = paste0("visualizations/histograms", clean_filename(col), "_histogram.png"),
         plot = p, width = 8, height = 5)
}

cat("✅ All visualizations have been saved in the 'visualizations' folder!\n")