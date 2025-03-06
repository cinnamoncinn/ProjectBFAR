# install.packages(c('readxl', 'ggplot2', 'dplyr', 'tidyr', 'readr', 'patchwork'))

# Load required libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# Define function to clean filenames (Windows-safe)
clean_filename <- function(filename) {
  gsub("[:?<>|*\"/\\\\]", "_", filename)  # Replace invalid characters with "_"
}

# Load dataset
df_A <- read_csv("Dataset/cleaned_data.csv", show_col_types = FALSE)

# Ensure 'A2:GROUP' exists and is a factor
if (!"A2:GROUP" %in% colnames(df_A)) {
  stop("Error: Column 'A2:GROUP' not found in dataset.")
}
df_A$`A2:GROUP` <- as.factor(df_A$`A2:GROUP`)

# Identify column types
categorical_cols <- names(df_A)[sapply(df_A, is.character) | sapply(df_A, is.factor)]
numerical_cols <- names(df_A)[sapply(df_A, is.numeric)]

# Remove 'A2:GROUP' from categorical list (since it's the grouping column)
categorical_cols <- setdiff(categorical_cols, "A2:GROUP")

# Create a long-format dataset for visualization
cat_data <- df_A %>%
  select(`A2:GROUP`, all_of(categorical_cols)) %>%
  pivot_longer(cols = all_of(categorical_cols), names_to = "Variable", values_to = "Value")

num_data <- df_A %>%
  select(`A2:GROUP`, all_of(numerical_cols)) %>%
  pivot_longer(cols = all_of(numerical_cols), names_to = "Variable", values_to = "Value")

# Create stacked bar charts for categorical variables
cat_plot <- ggplot(cat_data, aes(x = `A2:GROUP`, fill = Value)) +
  geom_bar(position = "fill") +
  facet_wrap(~Variable, scales = "free_x") +
  labs(title = "Categorical Variable Distribution by Group", x = "Group", y = "Proportion") +
  theme_minimal()

# Create histogram plots for numerical variables
num_plot <- ggplot(num_data, aes(x = Value, fill = `A2:GROUP`)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Numerical Variable Distribution by Group", x = "Value", y = "Count") +
  theme_minimal()

# Combine both plots using patchwork
library(patchwork)
combined_plot <- cat_plot / num_plot

# Save the visualization
ggsave("visualizations/all_variables_facet.png", plot = combined_plot, width = 12, height = 8)

# Show the plot
print(combined_plot)

cat("✅ Combined visualization saved as 'visualizations/all_variables_facet.png'!\n")
