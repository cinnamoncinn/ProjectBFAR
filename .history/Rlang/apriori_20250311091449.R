# Load required libraries
library(readr)
library(arules)
library(arulesViz)

# Load the dataset
file_path <- "dataset/cleaned_data.csv"  # Update this path
df <- read_csv(file_path)

# Select relevant columns (Parts D, F, G, H, I, J and A2:GROUP)
selected_parts <- c("D", "F", "G", "H", "I", "J")
selected_cols <- c("A2:GROUP", grep(paste0("^", selected_parts, collapse = "|"), names(df), value = TRUE))
df_selected <- df[selected_cols]

# Convert numeric values to categorical for transactions
for (col in names(df_selected)[-1]) {  # Exclude 'A2:GROUP'
  df_selected[[col]] <- ifelse(df_selected[[col]] > 0, "Yes", "No")
}

# Convert to factor to prevent errors
df_selected[-1] <- lapply(df_selected[-1], as.factor)

# Separate beneficiaries ("1") and non-beneficiaries ("0")
beneficiaries <- df_selected[df_selected$`A2:GROUP` == 1, -1]
non_beneficiaries <- df_selected[df_selected$`A2:GROUP` == 0, -1]

# Convert data to transactions format
trans_beneficiaries <- as(beneficiaries, "transactions")
trans_non_beneficiaries <- as(non_beneficiaries, "transactions")

# Apply Apriori algorithm
rules_beneficiaries <- apriori(trans_beneficiaries, parameter = list(support = 0.1, confidence = 0.5))
rules_non_beneficiaries <- apriori(trans_non_beneficiaries, parameter = list(support = 0.1, confidence = 0.5))

# View top rules
inspect(head(sort(rules_beneficiaries, by = "lift"), 5))
inspect(head(sort(rules_non_beneficiaries, by = "lift"), 5))

# Visualization
plot(rules_beneficiaries, method = "graph", engine = "igraph")
plot(rules_non_beneficiaries, method = "graph", engine = "igraph")
