# Load required libraries
library(arules)
library(ggplot2)
library(arulesViz)
library(plotly)

# Load dataset
file_path <- "Dataset/cleaned_data.csv"
df <- read.csv(file_path, stringsAsFactors = TRUE)

# Remove 'Age' column
df$B3.AGE <- NULL

# Function to bin income columns
bin_income <- function(column) {
  cut(as.numeric(column),
      breaks = c(0, 10000, 20000, 30000, 40000, 50000, Inf),
      labels = c("0-10K", "10K-20K", "20K-30K", "30K-40K", "40K-50K", "50K+"),
      include.lowest = TRUE)
}

# Apply binning
df$C1.TOT_INCOME.A <- bin_income(df$C1.TOT_INCOME.A)
df$C2.INCOME.B.FISH <- bin_income(df$C2.INCOME.B.FISH)
df$C4.INCOME.B.ALT <- bin_income(df$C4.INCOME.B.ALT)

# Convert dataset to transactions format
df_trans <- as(df, "transactions")

# Apply Apriori algorithm
rules <- apriori(df_trans, 
                 parameter = list(supp = 0.3, 
                                  conf = 0.6,  
                                  minlen = 2, 
                                  maxlen = 3))

# Convert rules to dataframe and save as CSV
rules_df <- as(rules, "data.frame")
write.csv(rules_df, file = "rules.csv", row.names = FALSE)

# Create APRIORI folder if it doesn't exist
output_folder <- "APRIORI"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Function to save plots
save_plot <- function(plot_func, filename) {
  filepath <- file.path(output_folder, filename)
  png(filepath, width = 800, height = 600)
  plot_func()
  dev.off()
}

# Save all plots
save_plot(function() plot(rules, method = "scatterplot", measure = c("support", "confidence"), shading = "lift"),
          "scatterplot.png")

save_plot(function() plot(rules, method = "graph", control = list(type = "items")),
          "graph.png")

save_plot(function() plot(rules, method = "grouped"),
          "grouped_plot.png")

save_plot(function() plot(rules, method = "paracoord", control = list(reorder = TRUE)),
          "parallel_coordinates.png")

top_rules <- head(sort(rules, by = "lift"), 10)  # Get top 10 rules
save_plot(function() plot(top_rules, method = "bar", measure = "lift", shading = "confidence"),
          "top_10_rules_bar.png")

# Interactive Graph (Cannot be saved as PNG)
htmlwidget_filepath <- file.path(output_folder, "graph_interactive.html")
htmlwidgets::saveWidget(plot(rules, method = "graph", engine = "htmlwidget"), htmlwidget_filepath)

print("Rules saved successfully in rules.csv")
print("Plots saved in the 'APRIORI' folder.")
