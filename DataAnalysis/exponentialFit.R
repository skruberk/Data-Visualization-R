#fits an exponential and plots the residuals
library(readr)
library(ggplot2)
library(minpack.lm)
# Extract the third column
data_values <- data[[3]]

# Remove any NA values
data_values <- na.omit(data_values)
bin_width <- 1  # Adjust this value as needed
# Create histogram

hist_data <- hist(data_values, breaks = seq(min(data_values), max(data_values), by = bin_width), plot = FALSE)
# Extract bin centers and counts
bin_centers <- hist_data$mids
bin_counts <- hist_data$counts

# Prepare data for fitting
fit_data <- data.frame(x = bin_centers, y = bin_counts)

# Define exponential model
exp_model <- function(b, x) {
  b[1] * exp(b[2] * x)
}


# Improved initial parameter guesses
initial_guess <- c(max(bin_counts), -1 / (max(bin_centers) - min(bin_centers)))


# Fit the model using nlsLM from minpack.lm package
fit <- nls.lm(y ~ b1 * exp(b2 * x), data = fit_data, start = list(b1 = initial_guess[1], b2 = initial_guess[2]))

# Extract fitted values
fit_data$fit <- predict(fit, newdata = fit_data)

# Compute residuals
fit_data$residuals <- fit_data$y - fit_data$fit

# Plot the histogram, the fitted curve, and the residuals
p <- ggplot(fit_data, aes(x = x)) +
  geom_bar(aes(y = y), stat = "identity", fill = "blue", alpha = 0.5) +
  geom_line(aes(y = fit), color = "red", size = 1) +
  labs(title = "Histogram with Exponential Fit", x = "Data Value", y = "Counts") +
  theme_minimal() +
  geom_point(aes(y = residuals), color = "green",alpha=0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") 

print(p)
# Display fitted parameters
summary(fit)

