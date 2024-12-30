
# install and load the required packages 
install.packages("reshape2")
library(reshape2)

# you will use dplyr for a few pre-processing steps
library(dplyr)

# read the dataset 
parking_df <- read.csv("parking.csv")

# structure of the DataFrame
str(parking_df)

head(parking_df)
# subset duplicated rows in the DataFrame 
duplicated_rows <- parking_df %>% filter(duplicated(parking_df))

nrow(duplicated_rows)

# remove 216 duplicate rows from the parking_df 
parking_df <- parking_df %>% distinct()

# number of rows in the updated DataFrame 
nrow(parking_df)

# convert the parking_df from wide to long format data
parking_melt <- melt(parking_df, id.vars = c("SystemCodeNumber", "LastUpdated"))

# display first 6 rows of the melted DataFrame
head(parking_melt)


# customize the default column headings of variable and value 
parking_melt <- melt(parking_df, id.vars = c("SystemCodeNumber", "LastUpdated"),
                     variable.name = "parking_variable", 
                     value.name = "parking_value")

# print the first 6 rows of updated parking_melt
head(parking_melt)


# convert the parking_melt from long to wide format data
parking_cast <- dcast(parking_melt, SystemCodeNumber + LastUpdated ~ parking_variable, value.var = "parking_value")

# display the first 6 rows of updated parking_cast 
head(parking_cast)

# read wholesale customers dataset
wholesale_df <- read.csv("Wholesale customers data.csv")

# structure of the DataFrame
str(wholesale_df)

# convert Region and Channel to factors with corresponding levels and labels
wholesale_df <- wholesale_df %>%
  mutate(Region = factor(Region, levels = c(1, 2, 3), 
                         labels = c("Lisbon", "Oporto", "Other Region")), Channel = factor(Channel, levels = c(1, 2), 
                                                                                           labels = c("Horeca", "Retail")))

# check the frequency of the columns Channel and Region
table(wholesale_df$Channel)

table(wholesale_df$Region)
# calculate the mean spending on each product category
spending_summary <- wholesale_df %>%
  summarise(mean_fresh = mean(Fresh),
            mean_milk = mean(Milk),
            mean_grocery = mean(Grocery),
            mean_frozen = mean(Frozen),
            mean_detergents_paper = mean(Detergents_Paper),
            mean_delicatessen = mean(Delicassen))

# print the summary
spending_summary

# group by region and calculate the mean spending on Fresh, Milk, and Grocery product category
grouped_data1 <- wholesale_df %>%
  group_by(Region) %>%
  summarise(mean_fresh = mean(Fresh),
            mean_milk = mean(Milk),
            mean_grocery = mean(Grocery))

# view the result
View(grouped_data1)

# group by channel and calculate the total spending on product categories
grouped_data2 <- wholesale_df %>%
  group_by(Channel) %>%
  summarise(total_fresh = sum(Fresh),
            total_milk = sum(Milk),
            total_grocery = sum(Grocery))

# view the result
View(grouped_data2)

# group by region and channel, and calculate the number of customers in each group
grouped_data3 <- wholesale_df %>%
  group_by(Region, Channel) %>%
  summarise(count = n())

# view the result
View(grouped_data3)

region_high_grocery_spending <- wholesale_df %>%
  group_by(Region) %>%
  filter(Grocery > mean(Grocery))

# Grouping by Region and calculating the total spending per region
region_total_spending <- wholesale_df %>%
  group_by(Region) %>%
  mutate(total_spending = sum(Fresh, Milk, Grocery, Frozen, Detergents_Paper, Delicassen))

# create a DataFrame
player_df1 <- data.frame(
  Player_Name = c("Virat Kohli", "Rohit Sharma", "Suresh Raina"),
  Score = c(102, 53, 45)
)

# create another DataFrame
player_df2 <- data.frame(
  Player_Name = c("Ravindra Jadeja", "Hardik Pandya"),
  Score = c(82, 64)
)

# combine the two DataFrames using rbind()
player_df_combined <- rbind(player_df1, player_df2)

# view the combined DataFrame
player_df_combined

# Create a DataFrame with characteristics of the match 
match_df <- data.frame(
  Venue = c("Mumbai", "Delhi", "Kolkata"),
  Date = c("2023-05-15", "2023-05-16", "2023-05-17")
)

# Combine player_df1 and match_df using cbind
combined_df <- cbind(player_df1, match_df)

# print combined_df
combined_df

# Create the transactions DataFrame
transactions <- data.frame(
  transaction_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  transaction_date = as.Date(c("2023-05-01", "2023-05-02", "2023-05-03", "2023-05-04", "2023-05-04", "2023-05-04", "2023-05-05", "2023-05-06", "2023-05-07")),
  transaction_amount = c(1050, 1075.50, 2050.00, 1120.00, 190.75, 1150.00, 160.00, 2580.50, 1825.00),
  customer_id = c(101, 102, 103, 104, 105, 106, 107, 108, 109),
  stringsAsFactors = FALSE
)
# print transactions DataFrame
transactions 

# create the customers DataFrame
customers <- data.frame(
  customer_id = c(102, 103, 104, 105, 107, 108, 109, 110),
  customer_name = c("Penny", "Sheldon", "Howard", "Amy", 
                    "Raj", "Stuart", "Emily", "Missy"),
  customer_email = c("penny@gmail.com", "sheldon@gmail.com","howard@gmail.com", "amy@gmail.com", "raj@gmail.com", NA, "emily@gmail.com", NA),
  stringsAsFactors = FALSE
)
# print customers DataFrame
customers


# perform an inner join
inner_join_df <- merge(transactions, customers, by = "customer_id")


# print inner_join_df
inner_join_df


# perform an outer join
outer_join_df <- merge(transactions, customers, by = "customer_id", all = TRUE)

# print outer_join_df
outer_join_df 

# perform a left join
left_join_df <- merge(transactions, customers, by = "customer_id", all.x = TRUE)

# print left_join_df
left_join_df 

# perform a right join
right_join_df <- merge(transactions, customers, by = "customer_id", all.y = TRUE)

# print right_join_df
right_join_df 


