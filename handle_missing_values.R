# Load required libraries
library(mice)       # for multiple imputation
library(VIM)        # for visualization

# Read the input data
data <- read.csv("input_data.csv")

# Function to handle missing values
handle_missing_values <- function(data) {
  # Create a copy of the data
  processed_data <- data
  
  # Calculate missing percentages
  missing_pct <- colMeans(is.na(data)) * 100
  
  # Strategy 1: Removal (if more than 50% missing)
  for(col in names(data)) {
    if (missing_pct[col] > 50) {
      processed_data[[col]] <- NULL
      cat(sprintf("\nRemoved column '%s' (%.1f%% missing)\n", col, missing_pct[col]))
    }
  }
  
  # Strategy 2: Imputation for numeric columns
  for(col in names(processed_data)) {
    if (is.numeric(processed_data[[col]])) {
      processed_data[[col]] <- ifelse(is.na(processed_data[[col]]),
                                    median(processed_data[[col]], na.rm = TRUE),
                                    processed_data[[col]])
      cat(sprintf("\nImputed numeric column '%s' with median\n", col))
    }
  }
  
  # Strategy 3: Replacement for categorical columns
  for(col in names(processed_data)) {
    if (is.factor(processed_data[[col]]) || is.character(processed_data[[col]])) {
      mode_value <- names(sort(table(processed_data[[col]]), decreasing = TRUE)[1])
      processed_data[[col]] <- ifelse(is.na(processed_data[[col]]),
                                    mode_value,
                                    processed_data[[col]])
      cat(sprintf("\nReplaced categorical column '%s' with mode\n", col))
    }
  }
  
  # Set random seed for reproducibility
  set.seed(123)
  
  # Perform multiple imputation for better results
  cat("\nPerforming multiple imputation for better accuracy...\n")
  imputed_data <- mice(processed_data, m = 5, method = "pmm", print = FALSE)
  final_data <- complete(imputed_data, 1)
  
  return(final_data)
}

# Set random seed for reproducibility
set.seed(123)

# Run the analysis
cat("\nProcessing missing values...\n")
processed_data <- handle_missing_values(data)

# Save the processed data
write.csv(processed_data, "output_data.csv", row.names = FALSE)

# Print summary
cat("\nSummary of changes:\n")
cat("\nOriginal missing values:\n")
print(colSums(is.na(data)))

cat("\nProcessed missing values:\n")
print(colSums(is.na(processed_data))) 

library(ggplot2)
library(reshape2)

# Select only numeric columns for calculating means
numeric_cols <- sapply(data, is.numeric)
numeric_col_names <- names(data)[numeric_cols]

# Calculate column means for only numeric columns
original_means <- sapply(numeric_col_names, function(col) mean(data[[col]], na.rm = TRUE))
processed_means <- sapply(numeric_col_names, function(col) mean(processed_data[[col]], na.rm = TRUE))

# Create data frame for plotting
plot_data <- data.frame(
  ColumnName = numeric_col_names,
  Original = original_means,
  Processed = processed_means
)

# Create a bar chart with simple approach
plot_data_melted <- melt(plot_data, id.vars = "ColumnName",
                          variable.name = "Data_Type", 
                          value.name = "Mean_Value")

# Create a bar chart comparing means
comparison_plot <- ggplot(plot_data_melted, aes(x = ColumnName, y = Mean_Value, fill = Data_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Mean Values Before and After Processing",
       x = "Column",
       y = "Mean Value",
       fill = "Data Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save plot
ggsave("comparison_plot.png", comparison_plot, width = 10, height = 6)

cat("\nVisualization created: comparison_plot.png\n")