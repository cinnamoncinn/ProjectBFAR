library(readxl)
library(ggplot2)

# Load dataset from Excel file (update the filename and sheet name)
file_path <- "partA.xlsx"
sheet_name <- "Sheet1"
data <- read_excel(file_path, sheet = sheet_name)

# Print first few rows
print(head(data))

# Summary statistics
print(summary(data))

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