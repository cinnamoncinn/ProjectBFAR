# Load required libraries
library(arules)

# Load dataset
file_path <- "Dataset/cleaned_data.csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)

# Remove 'Age' column
df$B3.AGE <- NULL

# Convert all columns to factors
df[] <- lapply(df, as.factor)

# Function to bin income columns
bin_income <- function(column) {
  cut(as.numeric(column),
      breaks = c(0, 10000, 20000, 30000, 40000, 50000, Inf),
      labels = c("0-10K", "10K-20K", "20K-30K", "30K-40K", "40K-50K", "50K+"),
      include.lowest = TRUE)
}

# Apply binning (with NA handling)
income_cols <- c("C1.TOT_INCOME.A", "C2.INCOME.B.FISH", "C4.INCOME.B.ALT")
for (col in income_cols) {
  df[[col]] <- bin_income(df[[col]])
  df[[col]][is.na(df[[col]])] <- "Unknown"
}

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

# Preview the high-confidence rules
View(high_conf_rules_df)  # Opens a new window in RStudio
# print(high_conf_rules_df)  # Alternative for console output

# Save filtered rules to CSV
write.csv(high_conf_rules_df, file = "high_conf_rules.csv", row.names = FALSE)

print("High-confidence rules saved successfully in high_conf_rules.csv")
