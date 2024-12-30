# install the lubridate package 
install.packages("lubridate")

# load the lubridate package
library(lubridate)

# view the current working directory
getwd()

# set your working directory
setwd("YOUR_PATH")

# define hours_watched_movies 
hours_watched_movies <- 6

# store the time spent (in hours) watching TV shows
hours_watched_tv <- 12

# save total time spent (in hours) into total_hours_watched 
total_hours_watched <-  18

# display the value stored in total_hours_watched 
total_hours_watched

# create a vector of durations of movies watched
movie_durations <- c(98, 109,124,138,115, 149)

# print movie_durations
movie_durations 

# create a vector of movies watched
movie_titles <- c("Rush Hour","Knight and Day", "The Notebook", "Shutter Island","Now you see me", "The Da Vinci Code")

# print movie_titles 
movie_titles 

# count the number of elements in the movie_titles
num_movie_titles <- length(movie_titles)

# print the result
cat("Number of movies watched on Netflix last month:", num_movie_titles )

# create a logical vector watched 
watched <- c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)

# print watched
watched 

# create a vector genre
genre <- c("Action", "Action", "Romance", "Thriller", "Thriller", "Thriller")

# check whether a movie belongs to a "Thriller" or not
watched_thriller <- genre == "Thriller"

# print watched_thriller
watched_thriller 

# create a vector rating
rating <- c("Thumbs Up", "Thumbs Down", "Thumbs Down", "Thumbs Up", "Thumbs Up", "Thumbs Up")

# check whether a movie is rated as "Thumbs Up" or not
watched_good <- rating == "Thumbs Up"

# print watched_good 
watched_good

# TV Shows watched
tv_shows_names <- c("Breaking Bad", "Money Heist", "The office", "Suits", "The Last Kingdom")
	
# time spent on watching each TV show in minutes
duration <- c(120, 142, 64, 90, 185)

# adding and accessing attributes
# assign names to duration
names(duration) <- tv_shows_names 

# print names and attributes of the vector duration
names(duration)

attributes(duration)

#  print duration
duration

# set the dimension of the vector
dim(duration) <- c(5, 1)

# print duration vector
duration

# create a matrix 
simple_matrix <- matrix(1:6, nrow = 3)
	
# print simple_matrix
simple_matrix

# create a vector with the duration of TV shows in hours
tv_duration <- c(2, 1, 2, 3)

# create a matrix with a number of hours spent watching each TV shows
tv_matrix <- matrix(tv_duration, nrow = 2, ncol = 2, byrow = TRUE)
	
# assign row and column names
# setting the vector tv_shows_names vector which we created earlier as rownames
	
rownames(tv_matrix) <- c("Breaking Bad", "Money Heist")
	
# assign column names
colnames(tv_matrix) <- c("Saturday", "Sunday")
	
# print tv_matrix
tv_matrix

# get the dimensions of the matrix
dim(tv_matrix)

# create a 3-dimensional array
duration_array <- array(data = c(30:39,50:59,20:29), 
          dim = c(5, 2, 3))


# note: the : operator in R is used to create a sequence of numbers. It takes two values, a starting value and an ending value, and generates a sequence of numbers that includes all the integers between those two values, inclusive.
# print duration_array 
duration_array 

# assign names to the dimensions of the array
dimnames(duration_array ) <- list(days = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5"),
                                                                  genre = c("Comedy", "Action"),
                                                                  device = c("TV", "Laptop", "Phone"))

# Print the array
duration_array 

# Start time of TV shows watched
jan_start_times <- as.POSIXct(c("2023-01-07 14:00:00", "2023-01-08 15:30:00", "2023-01-14 18:00:00", "2023-01-22 19:00:00"))

# End time of TV shows watched
jan_end_times <- as.POSIXct(c("2023-01-07 16:00:00", "2023-01-08 16:30:00", "2023-01-14 20:00:00", "2023-01-22 22:00:00"))

# calculate time spent in each show
jan_durations <- jan_end_times - jan_start_times

# print jan_durations
jan_durations 


# calculate the total duration
jan_total_duration <- sum(jan_durations)

# print jan_total_duration 
jan_total_duration 

# create a vector Movie Genres
movie_genres <- c("Action", "Action", "Romance", "Thriller", "Thriller", "Thriller")

# class of movie_genres 
class(movie_genres)
## "character"

# create a vector of movie rating
movie_ratings <- c(4,3,2,5,5,3)

# class of movie ratings 
class(movie_ratings)


# Create factor for Movie Genres
movie_genres_factor <- factor(movie_genres)

# class of movie ratings 
class(movie_genres_factor )
## "factor"

# Create factor for movie rating
movie_ratings_factor <- factor(movie_ratings)

# class of movie ratings 
class(movie_ratings_factor)


# View unique values in Movie Genres factor
levels(movie_genres_factor)

# View unique values in Movie Rating factor
levels(movie_ratings_factor)

# Assign custom labels to Movie Genres factor
levels(movie_genres_factor) <- c("Act", "Act", "Rom", "Thrill", "Thrill")

# Assign custom labels to Movie Ratings factor
levels(movie_ratings_factor) <- c("four","three","two","five","five","three")


# Create ordered factor for movie rating
movie_ratings_ordered_factor <- ordered(movie_ratings, levels = c(2, 3, 4, 5))

# print movie_ratings_ordered_factor 
movie_ratings_ordered_factor 

# View ordered levels in movie Rating factor
levels(movie_ratings_ordered_factor )

# Create a list of vectors
movies_list <- list(
  movie_names = c("Inception", "The Dark Knight", "Edge of Tomorrow"),
  movie_durations = c(148, 152, 113),
  genre = factor(c("Sci-fi", "Action","Sci-fi")), 
  ratings = factor(c(3,5,4), ordered = TRUE, levels = c(3,4,5))
)

# Print the list
movies_list

# create a DataFrame
netflix_df <- data.frame(
  movie_names = c("Inception", "The Dark Knight", "Edge of Tomorrow"),
  movie_durations = c(148, 152, 113),
  watched_scifi = c(TRUE, FALSE, TRUE)
)

# print netflix_df 
netflix_df

# class of the netflix_df
class(netflix_df)
# structure of the data frame netflix_df
str(netflix_df)

# create data frame
netflix_data <- data.frame(
  name = c("Aarya", "Vedh", "Vedika", "Raj", "Simran", "Rahul"),
  age = c(25, 27, 26, 24, 28, 25),
  gender = c("F", "M", "F", "M", "F", "M"),
  location = c("Kolkata", "Bangalore", "Bangalore", "Pune", "Chandigarh", "Mumbai"),
  total_hours = c(25, 20, 22, 9, 18, 21),
  hours_series = c(15, 10, 15, 7, 15, 18),
  hours_movies = c(9, 5, 8, 2, 2, 3),
  hours_documentaries = c(1, 2, 1, 0, 1, 0)
)

# view data frame
View(netflix_data)

# function to calculate total revenue
calculate_revenue <- function(price, num_sold) {
  total_revenue <- price * num_sold
  return(total_revenue)
}

# Define vectors to calculate revenue
price <- c(250, 2099, 1999, 750)
num_sold <- c(1000, 250, 100, 1150)

# Calculate the total revenue for each product
total_revenue <- calculate_revenue(price, num_sold)

# Print the results
total_revenue


# function to calculate conversion rate
calculate_conversion_rate <- function(impressions, clicks) {
  conversion_rate <- clicks/impressions
  return(conversion_rate)
}


# Define impressions and clicks vector
impressions <- c(1205, 2000, 1532, 4101)
clicks <- c(90, 75, 82, 210)

# Calculate conversion rates for each ad campaign
conversion_rates <- calculate_conversion_rate(impressions, clicks)

# Print the conversion_rates
conversion_rates



# function to calculate compound interest
calculate_compound_interest <- function(principal, interest_rate, years, compounding_frequency) {
  n <- compounding_frequency
  interest <- principal * (1 + interest_rate / n)^(n * years) - principal
  return(interest)
}

# define vector inputs
principal <- c(5000, 7500, 9200)
interest_rate <- c(0.05, 0.06, 0.09)
years <- c(1, 2, 3)
compounding_frequency <- c(1, 2, 4)


# calculate compound interest for each set of inputs
compound_interest <- calculate_compound_interest(principal, interest_rate, years, compounding_frequency)

# view output
compound_interest


# calculate total revenue (without return statement)
calculate_revenue <- function(price, num_sold) {
  price * num_sold
}


# display help page for the mean function
?mean
help(mean)

# quotes are optional
?"mean"
help("mean")

# read csv file
netflix_df <- read.csv("Aarya_Netflix.csv")

# read excel file
# read_excel("netflix_sample_excel.xlsx")

data()

# save data as a CSV file
write.csv(netflix_df, file = "Netflix_Data.csv")

# write.csv(netflix_df, file = "Netflix_Data.csv", row.names = FALSE)

# Create a data frame
my_data <- data.frame(x = 1:5, y = c("A", "B", "C", "D", "E"))

# Save data as a text file
write.table(my_data, file = "my_data.txt", sep = "\t", row.names = FALSE)

# create a sample data
my_data <- list(x = 1:5, y = c("A", "B", "C", "D", "E"))

# save data as a binary file
saveRDS(my_data, file = "my_data.rds")



















