library(ggplot2)

file_loaction = ""
# Load dataset
data <- mtcars

# Print first few rows
print(head(data))

# Summary statistics
print(summary(data))

# Histogram of MPG (Miles Per Gallon)
ggplot(data, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
  labs(title = "Histogram of MPG", x = "Miles Per Gallon", y = "Count")

# Boxplot of MPG grouped by number of cylinders
ggplot(data, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
  geom_boxplot() +
  labs(title = "Boxplot of MPG by Cylinders", x = "Number of Cylinders", y = "Miles Per Gallon")

# Scatter plot of MPG vs. Horsepower
ggplot(data, aes(x = hp, y = mpg)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "MPG vs Horsepower", x = "Horsepower", y = "Miles Per Gallon")
