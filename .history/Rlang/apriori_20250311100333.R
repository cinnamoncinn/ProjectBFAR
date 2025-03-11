library(readr)
library(arules)
library(arulesViz)

# Load the dataset
file_path <- "dataset/cleaned_data.csv"  # Update the filename if needed
df <- read_csv(file_path, show_col_types = FALSE)
#colnames(df)  # Show all column names in your dataset
colnames(df)[colnames(df) == "I3:???"] <- "I3:UNKNOWN"

# ✅ Define the exact column names
required_cols <- c(
  "A2:GROUP",   # Replace with the actual group column name if different
  
  # D Parts (Assets Owned)
  "D1.1:A_BIKE", "D1.2:A_MOTORC", "D1.3:A_TRICYCLE", "D1.4:A_CAR", "D1.5:A_JEEP", "D1.6:A_TRUCK", "D1.7:A_OTHERS",
  "D2.1:A_TV", "D2.2:A_DVD", "D2.3:A_WASH-M", "D2.4:A_AC", "D2.5:A_E-FAN", "D2.6:A_FRIDGE", "D2.7:A_STOVE", "D2.8:A_E-HEATER",
  "D2.9:A_FURNITURE", "D2.10:A_OTHERS", "D3.1:A_CP", "D3.2:A_LANDLINE", "D3.3:A_COMPUTER", "D3.4:A_OTHERS",

  # F Parts (Housing & Acquisition)
  "F1:A_HOUSE-OWN", "F2:A_HOUSE-ACQ", "F3:A_HOUSE-BUILT", "F4:A_OTHER-RP",

  # G Parts (Insurance & Security)
  "G1:A_SSS", "G2:A_GSIS", "G3:A_PhilHealth", "G4:A_PN-IN", "G5:A_LIFE-IN", "G6:A_HEALTH-IN",

  # H Parts (Government Assistance)
  "H1:4Ps", "H2:RET_P", "H3:SPES", "H4:AL_P", "H5:TBE", "H6:F_PC", "H7:AS_P", "H8:E/CW_P",

  # I Parts (Financial & Support Services)
  "I1:FD_Y", "I2:A/C_M", "I3:UNKNOWN", "I4:TFA", "I5:TFV", "I6.1:GN", "I6.2:FT", "I6.3:PPN", "I6.4:H&L", "I6.5:OTHERS"
)

# ✅ Check if all columns exist
missing_cols <- setdiff(required_cols, colnames(df))
if (length(missing_cols) > 0) {
  stop(paste("Error: Missing columns in dataset:", paste(missing_cols, collapse = ", ")))
}

# Select only the required columns
df_selected <- df[required_cols]

# Convert to factors for Apriori
df_selected[] <- lapply(df_selected, as.factor)

# Convert to transactions format
transactions <- as(df_selected, "transactions")

# Run Apriori Algorithm
rules <- apriori(transactions, parameter = list(supp = 1, conf = 0.1, maxlen = 50))

# Plot top rules
plot(rules, method = "graph", engine = "htmlwidget")
