#merge sort 
# Define the merge sort function
library(ggplot2)
# Define the merge sort function
i_vec <- seq(1, 50)
i_vec <- sample(i_vec)
print(i_vec)

merge_sort <- function(i_vec) {
  # Base case: If the vector has only one element, it is already sorted
  if (length(i_vec) <= 1) {
    return(i_vec)
  }
  
  # Recursive step: Divide the vector into two halves
  middle <- length(i_vec) %/% 2
  left <- merge_sort(i_vec[1:middle])
  right <- merge_sort(i_vec[(middle + 1):length(i_vec)])
  
  # Merge the sorted halves
  merged_vec <- merge(left, right)
  
  # Plot the merged vector to visualize the sorting process
  plot(merged_vec, type='o', main = "Merge Sort Visualization", xlab = "Index", ylab = "Number", col = rainbow(length(merged_vec)))
  Sys.sleep(0.1)  # Add a delay to visualize the sorting process
  
  return(merged_vec)
}

# Define the merge function to merge two sorted vectors
merge <- function(left, right) {
  result <- numeric(length(left) + length(right))
  i <- 1
  j <- 1
  k <- 1
  
  # Compare elements from left and right vectors and merge them into the result
  while (i <= length(left) && j <= length(right)) {
    if (left[i] <= right[j]) {
      result[k] <- left[i]
      i <- i + 1
    } else {
      result[k] <- right[j]
      j <- j + 1
    }
    k <- k + 1
  }
  
  # Copy any remaining elements from left vector
  while (i <= length(left)) {
    result[k] <- left[i]
    i <- i + 1
    k <- k + 1
  }
  
  # Copy any remaining elements from right vector
  while (j <= length(right)) {
    result[k] <- right[j]
    j <- j + 1
    k <- k + 1
  }
  
  return(result)
}

# Example usage
vec <- merge_sort(i_vec)
print(vec)
