# Load necessary libraries
library(arules)
library(arulesViz)
library(dplyr)

# Load dataset
data <- read.csv("cleaned_data.csv", stringsAsFactors = TRUE)

# Select relevant columns for transaction analysis
selected_columns <- c(
  "D1.1.A_BIKE", "D1.2.A_MOTORC", "D1.3.A_TRICYCLE", "D1.4.A_CAR", "D1.5.A_JEEP", "D1.6.A_TRUCK", "D1.7.A_OTHERS",
  "D2.1.A_TV", "D2.2.A_DVD", "D2.3.A_WASH.M", "D2.4.A_AC", "D2.5.A_E.FAN", "D2.6.A_FRIDGE", "D2.7.A_STOVE", 
  "D2.8.A_E.HEATER", "D2.9.A_FURNITURE", "D2.10.A_OTHERS", "D3.1.A_CP", "D3.2.A_LANDLINE", "D3.3.A_COMPUTER", "D3.4.A_OTHERS"
)
transactions <- data[selected_columns]

# Convert TRUE/FALSE to transaction format
transactions[] <- lapply(transactions, function(x) ifelse(x == TRUE, as.character(sub(".*:", "", names(x))), NA))
transactions <- as(as.data.frame(transactions), "transactions")

# Apply Apriori algorithm with optimized parameters
rules <- apriori(transactions, parameter = list(supp = 0.03, conf = 0.6, maxlen = 5))

# Visualize the rules
plot(rules, method="graph", engine="igraph")
