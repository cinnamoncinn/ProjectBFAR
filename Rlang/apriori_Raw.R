# Load required libraries
library(arules)
library(readxl)  # For reading Excel files

# Load dataset from Excel
file_path <- "Dataset/BFAR-DATASET2.xlsx"
df <- read_excel(file_path, sheet = "FINAL_DATASET")

# Convert column names to valid R format
colnames(df) <- make.names(colnames(df))

# Remove 'B3.AGE' column
df$B3.AGE <- NULL

# Function to bin income columns
bin_income <- function(column) {
  cut(as.numeric(column),
      breaks = c(0, 10000, 20000, 30000, 40000, 50000, Inf),
      labels = c("0-10K", "10K-20K", "20K-30K", "30K-40K", "40K-50K", "50K+"),
      include.lowest = TRUE)
}

# Detect income columns dynamically
income_cols <- grep("INCOME", colnames(df), value = TRUE)

# Apply binning and handle NAs
for (col in income_cols) {
  df[[col]] <- bin_income(df[[col]])
  df[[col]][is.na(df[[col]])] <- "Unknown"
}

# Convert all columns to factors
df[] <- lapply(df, as.factor)

# Convert dataset to transactions format
df_trans <- as(df, "transactions")

# Apply Apriori algorithm
rules <- apriori(df_trans, 
                 parameter = list(supp = 0.1, 
                                  conf = 0.6,  
                                  minlen = 2, 
                                  maxlen = 3))

# Convert rules to dataframe
rules_df <- as(rules, "data.frame")

# Filter rules with confidence >= 0.8
high_conf_rules <- subset(rules, quality(rules)$confidence >= 0.8)

# Convert filtered rules to dataframe
high_conf_rules_df <- as(high_conf_rules, "data.frame")

# Preview the high-confidence rules in a new window
View(high_conf_rules_df)  # Opens a new window in RStudio

# Save filtered rules to CSV
write.csv(high_conf_rules_df, file = "high_conf_rules.csv", row.names = FALSE)

print("High-confidence rules saved successfully in high_conf_rules.csv")
