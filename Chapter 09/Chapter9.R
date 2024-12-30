
library(dplyr)

# Generate a population of 100 students
population <- 1:100

# Select a simple random sample of 5 students with replacement
sample_with_replacement <- sample(population, size = 5, replace = TRUE)

# Display the sample
print(sample_with_replacement)

# Select a simple random sample of 5 students without replacement
sample_without_replacement <- sample(population, size = 5, replace = FALSE)

# Display the sample
print(sample_without_replacement)

# Create a dataset with customer information
set.seed(123)  # For reproducibility
customers <- data.frame(
  customer_id = 1:1000,
  age_group = sample(c("18-25", "26-35", "36-45", "46+"), 1000, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
  shopping_frequency = sample(c("occasional", "regular", "frequent"), 1000, replace = TRUE, prob = c(0.4, 0.3, 0.3))
)

# Stratified sampling using dplyr
stratified_sample <- customers %>%
  group_by(age_group, shopping_frequency) %>%
  sample_n(size = 20, replace = FALSE) 

table(stratified_sample$age_group, stratified_sample$shopping_frequency)

# Create a dataframe
delivery_data <- data.frame(
  delivery_times = c(18, 20, 22, 25, 28, 30, 3),
  package_types = c("Small", "Large", "Medium", "Large", "Small", "Medium", "Small")
)

# Calculate mean
mean_delivery_time <- mean(delivery_data$delivery_times)
cat("Mean Delivery Time:", mean_delivery_time, "hours")

# Calculate median
median_delivery_time <- median(delivery_data$delivery_times)
cat("Median Delivery Time:", median_delivery_time, "hours")

# Function to calculate mode  
get_mode <- function(x) {
  unique_values <- unique(x)
  frequencies <- tabulate(match(x, unique_values))
  mode_value <- unique_values[which.max(frequencies)]
  return(mode_value)
}

# Calculate mode for package types
mode_package_type <- get_mode(delivery_data$package_types)
cat("Mode Package Type:", mode_package_type)


# Create a vector, daily_visits
daily_visits <- c(1200, 950, 1500, 800, 1100, 1350, 1000, 1800, 750, 1400)

# Calculate Range
visits_range <- diff(range(daily_visits))
cat("Range of daily website visits:", visits_range, "visits\n")

# Calculate Variance
visits_variance <- var(daily_visits)
cat("Variance of daily website visits:", visits_variance, "visits^2\n")

# Calculate Standard Deviation
visits_std_dev <- sd(daily_visits)
cat("Standard Deviation of daily website visits:", visits_std_dev, "visits\n")

