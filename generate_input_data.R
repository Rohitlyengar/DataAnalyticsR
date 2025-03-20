# Generate input data with missing values
library(datasets)

# Get the iris dataset
data(iris)
iris_df <- as.data.frame(iris)

# Save the original data without any missing values
write.csv(iris_df, "original_data.csv", row.names = FALSE)

# Add 10% missing values randomly
set.seed(123)  # for reproducibility
for(col in names(iris_df)) {
  n_missing <- floor(0.1 * nrow(iris_df))
  missing_rows <- sample(1:nrow(iris_df), n_missing)
  iris_df[missing_rows, col] <- NA
}

# Save the dataset with missing values
write.csv(iris_df, "input_data.csv", row.names = FALSE)

# Print summary of missing values
cat("\nMissing values in input data:\n")
print(colSums(is.na(iris_df))) 