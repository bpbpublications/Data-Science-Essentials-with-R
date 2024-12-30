
# load the packages
library(dplyr)
library(ggplot2)

# Read the dataset
forest_fires <- read.csv("Algerian_forest_fires.csv")

############### PRE-PROCESSING ###############

# Convert specified columns from chr to numeric
forest_fires[, c("Temperature", "RH", "Ws", "Rain", "FFMC", "DMC", "DC", "ISI", "BUI", "FWI")] <- 
  lapply(forest_fires[, c("Temperature", "RH", "Ws", "Rain", "FFMC", "DMC", "DC", "ISI", "BUI", "FWI")], as.numeric)

forest_fires <- subset(forest_fires, day != "day")

forest_fires <- na.omit(forest_fires)

forest_fires$Classes  <- str_trim(forest_fires$Classes)

forest_fires$Classes <- as.factor(forest_fires$Classes)


##############################################


# Distribution of the Temperature
ggplot(forest_fires, aes(x = Temperature)) +
  geom_histogram(binwidth = 1,fill = "steelblue3", color = "black", boundary = 0) +
  labs(x = "Temperature (in Celsius Degrees)",
       y = "Frequency") +
  theme_bw() 


# Z-Score scaling function
z_score_scaling <- function(x) {
  (x - mean(x)) / sd(x)
}

# Apply Z-Score scaling to the Temperature column
forest_fires$Temperature_Z_Score <- z_score_scaling(forest_fires$Temperature)

# Distribution of the Scaled Temperature
ggplot(forest_fires, aes(x = Temperature_Z_Score)) +
  geom_histogram(binwidth = 1, fill = "steelblue3", color = "black", boundary= 0) +
  labs(x = "Scaled Temperature (Z-Scores)",
       y = "Frequency") +
  theme_bw() 


# Min-Max scaling function
min_max_scaling <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply Min-Max scaling to the Temperature column
forest_fires$Temperature_Scaled <- min_max_scaling(forest_fires$Temperature)

# Distribution of the Scaled Temperature
ggplot(forest_fires, aes(x = Temperature_Scaled)) +
  geom_histogram(binwidth = 0.125, fill = "steelblue3", color = "black", boundary = 0) +
  labs(x = "Scaled Temperature",
       y = "Frequency") +
  theme_bw() 


# Define the number of bins
num_bins <- 4

# Create bins for the temperature data
forest_fires$temp_bins <- cut(forest_fires$Temperature, breaks = num_bins, labels = FALSE)

# Convert temp_bins to factor
forest_fires$temp_bins <- as.factor(forest_fires$temp_bins)

# Distribution of the Temperature with bin lines
ggplot(forest_fires, aes(x = Temperature, fill = temp_bins)) +
  geom_histogram(binwidth = 1, color = "black", boundary = 0) +
  geom_vline(xintercept = seq(min(forest_fires$Temperature), max(forest_fires$Temperature), length.out = num_bins + 1),
             linetype = "dashed", color = "darkred", linewidth = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Temperature",
       y = "Frequency",
       fill = "Temperature Bins") +
  theme_bw()



# Perform quantile binning on the Temperature column
forest_fires$temperature_quantile_bins <- cut(forest_fires$Temperature, 
                                              breaks = quantile(forest_fires$Temperature),
                                              labels = FALSE,include.lowest = TRUE)

# Convert quantile bins to factor
forest_fires$temperature_quantile_bins <- as.factor(forest_fires$temperature_quantile_bins)


# Create histogram with quantile binning
ggplot(forest_fires, aes(x = Temperature, fill = temperature_quantile_bins)) +
  geom_histogram(binwidth = 1, color = "black", boundary = 0) +
  geom_vline(xintercept = quantile(forest_fires$Temperature),
             linetype = "dashed", color = "darkred", linewidth = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Temperature",
       y = "Frequency",
       fill = "Quantile Bins") +
  theme_bw()



# Distribution of the Rain
ggplot(forest_fires, aes(x = Rain)) +
  geom_histogram(binwidth = 1,fill = "steelblue3", color = "black", boundary = 0) +
  labs(x = "Rain (in mm)",
       y = "Frequency") +
  theme_bw() 

# Create a column, Rain_Capped
forest_fires <- forest_fires %>%
  mutate(Rain_Capped = ifelse(Rain > 5, 5, Rain))

# Distribution of the Capped Rain
ggplot(forest_fires, aes(x = Rain_Capped)) +
  geom_histogram(binwidth = 1,fill = "steelblue3", color = "black", boundary = 0) +
  labs(x = "Rain (in mm)",
       y = "Frequency") +
  theme_bw() 

# Create a sample dataframe 
customer_category <- data.frame(
  customer_id = 1:7,
  customer_segment = c("First-time purchasers", "Loyal customers", "Loyal customers", "Lapsed purchasers", 
                       "First-time purchasers", "Lapsed purchasers", "Loyal customers")
)

# Define a label encoding function
label_encode <- function(x) {
  levels <- unique(x)
  as.numeric(factor(x, levels = levels))
}

# Create a new column with label encoding
customer_category <- customer_category %>%
  mutate(customer_segment_encoded = label_encode(customer_segment))

View(customer_category)

library(fastDummies)

# Perform one-hot encoding using dummy_cols()
customer_category_encoded <- dummy_cols(customer_category, select_columns = "customer_segment")

View(customer_category_encoded)

# Perform dummy encoding
customer_category_encoded <- dummy_cols(customer_category, select_columns = "customer_segment", remove_first_dummy = TRUE)

View(customer_category_encoded)


