# Install required package if not installed
library(arules)
library(ggplot2)
library(arulesViz)
# Load dataset
file_path <- "Dataset/cleaned_data.csv"
df <- read.csv(file_path, stringsAsFactors = TRUE)

# Remove 'Age' column
df$B3.AGE <- NULL
df
