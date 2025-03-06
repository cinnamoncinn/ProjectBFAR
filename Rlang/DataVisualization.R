<<<<<<< Updated upstream
=======
# Download the ff packages
# install.packages(c('readxl', 'ggplot2', 'dplyr', 'tidyr', 'readr'))
# ----------------------------------------
# Load required libraries
# ----------------------------------------
>>>>>>> Stashed changes
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
# ----------------------------------------
# Define function to clean filenames (Windows-safe)
# ----------------------------------------
clean_filename <- function(filename) {
  gsub("[:?<>|*\"/\\\\]", "_", filename)  # Replace invalid characters with "_"
}

<<<<<<< Updated upstream
# Load dataset from Excel file (update the filename and sheet name)
file_path <- "partA.xlsx"
sheet_name <- "Sheet1"
data <- read_excel(file_path, sheet = sheet_name)
=======
# ----------------------------------------
# Load dataset
# ----------------------------------------
df_A <- read_csv("Dataset/cleaned_data.csv", show_col_types = FALSE)
>>>>>>> Stashed changes

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

<<<<<<< Updated upstream
# Histogram (Replace 'your_column' with the column you want to visualize)
ggplot(data, aes(x = your_column)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Your Column", x = "Your Column", y = "Count")

# Boxplot (Replace 'category_column' and 'value_column' accordingly)
ggplot(data, aes(x = factor(category_column), y = value_column, fill = factor(category_column))) +
  geom_boxplot() +
  labs(title = "Boxplot of Value Column by Category", x = "Category", y = "Value")

# Scatter plot (Replace 'x_column' and 'y_column')
ggplot(data, aes(x = x_column, y = y_column)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of X vs Y", x = "X Column", y = "Y Column")
=======
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
    theme_minimal()
  
  ggsave(filename = paste0("visualizations/", clean_filename(col), "_stacked_bar.png"),
         plot = p, width = 8, height = 5)
}

# ----------------------------------------
# Generate Box Plots (Numerical Variables)
# ----------------------------------------
for (col in numerical_cols) {
  p <- ggplot(df_A, aes(x = `A2:GROUP`, y = get(col), fill = `A2:GROUP`)) +
    geom_boxplot() +
    labs(title = paste("Distribution of", col, "by Group"), x = "Group", y = col) +
    theme_minimal()
  
  ggsave(filename = paste0("visualizations/", clean_filename(col), "_boxplot.png"),
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
    theme_minimal()
  
  ggsave(filename = paste0("visualizations/", clean_filename(col), "_histogram.png"),
         plot = p, width = 8, height = 5)
}

# ----------------------------------------
# Done! All visualizations are saved in the 'visualizations' folder
# ----------------------------------------
cat("✅ All visualizations have been saved in the 'visualizations' folder!\n")
>>>>>>> Stashed changes
