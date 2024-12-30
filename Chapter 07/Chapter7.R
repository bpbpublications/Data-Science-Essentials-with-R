
# Load tidyverse:
library(tidyverse)

# Alternatively, install ggplot2
install.packages("ggplot2")

install.packages("forcats")

# Load ggplot2
library(ggplot2)
library(forcats)


# Read the dataset
spice_consumption <- read.csv("Spice_Consumption.csv")

# Arrange the data in descending order of Value and subset the top 10 countries 
spice_top10 <- spice_consumption %>% 
  arrange(desc(Value)) %>%
  slice_head(n = 10)

# Distribution of Spice Consumption
ggplot(spice_top10, aes(x = Country, y = Value)) +
  geom_bar(stat = "identity")

# Distribution of Spice Consumption
ggplot(spice_top10, aes(x = Country, y = Value)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Distribution of Spice Consumption
ggplot(spice_top10, aes(x = Country, y = Value)) +
  geom_bar(stat = "identity") +
  coord_flip()


# Distribution of Spice Consumption
ggplot(spice_top10, aes(x = fct_reorder(Country, Value), y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue3") +
  labs(title = "Spice Consumption",
       x = "Country",
       y = "Spice Consumption (kt)") + 
  coord_flip() +
  theme_bw()


# Read the dataset
undernourishment <- read.csv("undernourishment_data.csv")

# Subsetting data for 2021
undernourishment_2021 <- undernourishment %>% filter(Year == 2021)

# Distribution of the Undernourishment Prevalence
ggplot(undernourishment_2021, aes(x = Value)) +
  geom_histogram(binwidth =2, fill = "steelblue3", color = "black") +
  labs(title = "Prevalence of Undernourishment",
       x = "Prevalence (%)",
       y = "Frequency") +
  theme_bw() 


# Distribution of the Undernourishment Prevalence
ggplot(undernourishment_2021, aes(x = Value)) +
  geom_histogram(binwidth =2, fill = "steelblue3", color = "black") +
  labs(title = "Prevalence of Undernourishment",
       x = "Prevalence (%)",
       y = "Frequency") +
  stat_bin(aes(label = after_stat(count)), 
           binwidth = 2,
           geom = "text", vjust = -0.5, color = "black", size = 3) +
  theme_bw() 

# Density Plot of Undernourishment Prevalence
ggplot(undernourishment_2021, aes(x = Value)) +
  geom_density(fill = "lightblue", color = "steelblue4") +
  labs(title = "Prevalence of Undernourishment",
       x = "Prevalence (%)",
       y = "Frequency") +
  theme_bw() 


hunger_2021 <- read.csv("hunger_2021.csv")

# Pie chart
ggplot(hunger_2021, aes(x = "", y = Value, fill = Region)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y")


# Pie Chart displaying the undernourished population across different regions
ggplot(hunger_2021, aes(x = "", y = Value, fill = fct_reorder(Region, Value))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +  
  theme(legend.position = "bottom") +
  labs(title = "Undernourished People by Region", fill = "Region") +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(x = 1.7, label = ifelse(Value >= 30, paste(Region, "\n", round(Value, 0), "M"), "")), 
            position = position_stack(vjust = 0.5), 
            size = 3, show.legend = FALSE)


# Create a new column, "Percentage"
hunger_2021 <- hunger_2021 %>%
  mutate(Percentage = round((Value / sum(Value)) * 100,1))

# Stacked Bar Chart
ggplot(hunger_2021, aes(x = "", y = Percentage, 
                        fill = fct_reorder(Region, Percentage))) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = ifelse(Percentage >= 1, paste0(round(Percentage, 0), "%"), "")), 
            position = position_stack(vjust = 0.5), 
            size = 3, show.legend = FALSE) +
  labs(title = "Undernourished People by Region", fill = "Region",
       x = "", y = "Percentage") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw()


cab_rides <- read.csv("cab_rides.csv")

cab_rides$RatecodeID <- as.factor(cab_rides$RatecodeID)

cab_rides <- cab_rides %>%
  mutate(pickup_date = as_date(tpep_pickup_datetime))

# Create a boxplot to visualise the driving speed distribution
ggplot(cab_rides, aes(x = "", y = speed)) +
  geom_boxplot() +
  labs(
    title = "Driving Speed Distribution",
    x = "",
    y = "Speed (mph)"
  ) +
  theme_bw()

# Create a function to calculate the time of the day
time_of_day <- function(x) {
  if (x %in% 6:11) {
    return('Morning')
  } else if (x %in% 12:16) {
    return('Afternoon')
  } else if (x %in% 17:21) {
    return('Evening')
  } else {
    return('Night')
  }
}

# Create a new column- pickup_timeofday
cab_rides <- cab_rides %>%
  mutate(pickup_timeofday = sapply(pickup_hour, time_of_day))

# Create pickup_timeofday, an ordered factor type 
cab_rides$pickup_timeofday <- factor(cab_rides$pickup_timeofday, levels = c("Morning", "Afternoon", "Evening", "Night"), ordered = TRUE)

# Create a boxplot to visualize the speed distribution by the time of the day
ggplot(cab_rides, aes(x = pickup_timeofday, y = speed)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Speed by Time of Day",
    x = "Time of Day",
    y = "Speed"
  ) +
  theme_bw()


# The relationship between Trip Distance and the Fare amount
ggplot(cab_rides, aes(x = trip_distance, y = fare_amount)) +
  geom_point(col = "steelblue4", alpha = 0.5) +
  labs(
    title = "Trip Distance v/s Fare Amount",
    x = "Distance (Miles)",
    y = "Amount (USD)"
  ) +
  theme_bw()


# The relationship between Trip Distance and the Fare amount by RatecodeID
ggplot(cab_rides, aes(x = trip_distance, y = fare_amount)) +
  geom_point(aes(col = RatecodeID, shape = RatecodeID), alpha = 0.6) +
  labs(
    title = "Trip Distance v/s Fare Amount",
    x = "Distance (Miles)",
    y = "Amount (USD)"
  ) +
  theme_bw()


# The relationship between Trip Distance and the Fare amount by RatecodeID
ggplot(cab_rides, aes(x = trip_distance, y = fare_amount)) +
  geom_point(aes(col = RatecodeID, shape = RatecodeID), alpha = 0.6) +
  labs(
    title = "Trip Distance v/s Fare Amount",
    x = "Distance (Miles)",
    y = "Amount (USD)"
  ) +
  facet_grid(RatecodeID~.) +
  theme_bw()


# Create a summarised dataset for analysis
total_trips_per_day <- cab_rides %>%
  group_by(pickup_date) %>%
  summarize(total_trips = n())

# Line chart visualizing Total trips on January 2023
ggplot(total_trips_per_day, aes(x = pickup_date, y = total_trips)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Total Trip Distribution on January 2023",
    x = "Pickup Date",
    y = "Total Trips"
  ) +
  theme_bw()


