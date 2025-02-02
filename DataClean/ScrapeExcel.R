library(readr)

# Import the CSV file and change the name of the column to the value
column_name <- "eGFP_noCCCP"

# Flatten the dataframe into a single vector by collapsing rows into one string and splitting by spaces
flat_data <- unlist(strsplit(paste(data$X1, collapse = " "), "\\s+"))  # X1 is the column name when col_names = FALSE
# Find all occurrences of "Spearman's rank correlation value"
matches <- grep("Spearman's", flat_data,fixed = TRUE)

# Print matches to debug
print(matches)
# Inspect raw byte values
#print(charToRaw("correlation value"))

# Extract the next four numerical values after each occurrence
extracted_values <- unlist(lapply(matches, function(idx) {
  # Ensure we don't go beyond the vector length
  next_values <- flat_data[(idx + 1):(idx + 29)]
  # Keep only numeric values
  next_values <- next_values[grepl("^[-+]?[0-9]*\\.?[0-9]+$", next_values)]
  return(next_values)
}))
print(extracted_values)
# Convert extracted values into a data frame with the specified column name
result_df <- data.frame(eGFP_noCCCP = extracted_values, stringsAsFactors = FALSE)
View(result_df)
# Remove values greater than 0.9 (keeping only those <= 0.9)
result_df <- result_df[result_df <= 0.9]
result_df <- data.frame(eGFP_noCCCP = result_df, stringsAsFactors = FALSE)
# Write to a new CSV file
write_csv(result_df, "eGFP_noCCCP_out.csv")

print("Extraction complete. Check 'extracted_values.csv'.")

write_csv(as.data.frame(result_df), "extracted_values.csv")

print("Extraction complete. Check 'extracted_values.csv'.")
