# Load dataset
dataset <- read.csv("dataset/cleaned_data.csv")

# Convert dataset to a transactions object
dataset_trans <- as(dataset, "transactions")

# Apply Apriori algorithm
rules <- apriori(dataset_trans, 
                 parameter = list(supp = 0.01, conf = 0.5, minlen = 2))

# View the top rules
inspect(head(rules, 10))

# Visualize the rules
plot(rules, method="graph", engine="htmlwidget")