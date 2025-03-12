# Install required package if not installed
library(arules)
library(ggplot2)
library(arulesViz)

# Load dataset
file_path <- "Dataset/cleaned_data.csv"
df <- read.csv(file_path, stringsAsFactors = TRUE)

# Remove 'Age' column
df$B3.AGE <- NULL

# Bin Income columns into ranges
bin_income <- function(column) {
  cut(as.numeric(column),
      breaks = c(0, 10000, 20000, 30000, 40000, 50000, Inf),
      labels = c("0-10K", "10K-20K", "20K-30K", "30K-40K", "40K-50K", "50K+"),
      include.lowest = TRUE)
}

df$C1.TOT_INCOME.A <- bin_income(df$C1.TOT_INCOME.A)
df$C2.INCOME.B.FISH <- bin_income(df$C2.INCOME.B.FISH)
df$C4.INCOME.B.ALT <- bin_income(df$C4.INCOME.B.ALT)

# Convert dataset to transactions format
df_trans <- as(df, "transactions")

# Apply Apriori algorithm
rules <- apriori(df_trans, 
                 parameter = list(supp = 0.3, # 0.1 to 0.2 is ideal for BIG dataset
                                  conf = 0.6,  # 0.7 to 0.8 is ideal for BIG dataset
                                  minlen = 2, # 1 or 2 is ideal for BIG dataset
                                  maxlen = 3)) # 3 is ideal for BIG dataset 

# Convert rules to dataframe and save as CSV
rules_df <- as(rules, "data.frame")
write.csv(rules_df, file = "rules.csv", row.names = FALSE)
plot(rules, method = "scatterplot", measure = c("support", "confidence"), shading = "lift")

print("Rules saved successfully in rules.csv")
