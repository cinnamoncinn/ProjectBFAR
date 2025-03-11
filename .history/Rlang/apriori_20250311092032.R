# Load required libraries
library(readr)
library(arules)
library(arulesViz)

# Load the dataset (Explicitly define column types)
file_path <- "dataset/cleaned_data.csv"  # Update path if needed
df <- read_csv(file_path, show_col_types = FALSE)

# Convert column names to valid R format
colnames(df) <- make.names(colnames(df))

# Select relevant columns (Parts D, F, G, H, I, J and A2:GROUP)
selected_cols <- c("Correct_Group_Column", 
                   "D1.1:A_BIKE", "D1.2:A_MOTORC", ..., "D3.4:B_OTHERS", 
                   "F1:A_HOUSE-OWN", ..., "F4:B_OTHER-RP", 
                   "G1:A_SSS", ..., "G6:B_HEALTH-IN", 
                   "H1:4Ps", ..., "H8:E/CW_P",
                   "I1:FD_Y", ..., "I6.5:OTHERS")

selected_cols <- c("A2.GROUP", intersect(names(df), selected_parts))  # Ensure columns exist
df_selected <- df[selected_cols]

# Check if all required columns exist
if (length(selected_cols) <= 1) {
  stop("Error: Selected columns are missing. Check column names in dataset.")
}

# Convert A2:GROUP to a factor
df_selected$A2.GROUP <- as.factor(df_selected$A2.GROUP)

# Convert numeric values to categorical ("Yes"/"No")
for (col in names(df_selected)[-1]) {  # Exclude 'A2.GROUP'
  df_selected[[col]] <- ifelse(is.na(df_selected[[col]]), "No", 
                               ifelse(as.numeric(df_selected[[col]]) > 0, "Yes", "No"))
}

# Convert all columns to factor (Mandatory for transactions)
df_selected[-1] <- lapply(df_selected[-1], as.factor)

# Check for inconsistencies before converting to transactions
if (any(sapply(df_selected[-1], function(col) length(unique(col))) == 1)) {
  stop("Error: Some columns have only one unique value. Check data.")
}

# Separate Beneficiaries (1) and Non-Beneficiaries (0)
beneficiaries <- df_selected[df_selected$A2.GROUP == 1, -1]
non_beneficiaries <- df_selected[df_selected$A2.GROUP == 0, -1]

# Convert to transactions
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
