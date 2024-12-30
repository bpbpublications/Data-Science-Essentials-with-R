# load the required packages
install.packages("dslabs")
library(dslabs)

# install.packages("tidyverse")
library(tidyverse)

# load the data
data(gapminder)

# To get familiar with this dataset, you might want to use the below function
?gapminder

# check the data type of gapminder dataset
class(gapminder)

# display the structure of the gapminder dataset
str(gapminder)

# display all columns of the dataframe
colnames(gapminder)

dim(gapminder)


# check the number of rows in the dataset
nrow(gapminder)


# check the number of columns in the dataset
ncol(gapminder)

# rename the population and GDP column names
gapminder <- rename(gapminder, country_population = population , country_gdp = gdp )


# select the country, year, infant mortality, and life expectancy variables 
gapminder_subset <- select(gapminder, country, year, infant_mortality, life_expectancy)

# the equivalent command in base R
gapminder_subset <- gapminder[, c("country", "year", "infant_mortality", "life_expectancy")]


# select all columns between country and life_expectancy (inclusive)
gapminder_subset2 <- select(gapminder, country:life_expectancy)


# select all columns that start with "country"
gapminder_subset3 <- select(gapminder, starts_with("country"))

# check column names of the dataframe gapminder_subset3
colnames(gapminder_subset3)

# select all columns except continent and region
gapminder_subset4 <- select(gapminder, -continent, -region)


# display the unique years present in the dataset
unique(pull(gapminder,year))

# store the unique years present in a dataframe
unique_years  <- unique(select(gapminder,year))

# Here, the result is stored in the dataframe unique_years and it can be displayed using View()
View(unique_years)

# For what years have the data been collected?
length(unique(pull(gapminder,year)))


# filter the countries in Asia with a life expectancy > 70 years in 2015
gapminder_asia <- filter(gapminder, year == 2015, continent == "Asia", life_expectancy > 70) 


# create a subset of the gapminder dataset that only contains countries from the Americas and Asia continents.
gapminder_asia_america <- filter(gapminder, continent == "Americas" | continent == "Asia")


gapminder_asia_america2 <- filter(gapminder, continent %in% c("Americas","Asia"))

gapminder_asia_europe <- gapminder %>% 
  filter(year == 2010, continent %in% c("Asia", "Europe"), life_expectancy > 70) %>% 
  select(country, continent, country_population, country_gdp, infant_mortality, life_expectancy)


# subset countries with a population > 100 million in 2015
gapminder_country2015 <- gapminder %>%
  filter(year == 2015, country_population > 100000000) %>%
  select(country)

View(gapminder_country2015)


# identify unique continents in the gapminder dataset
unique_continents <- gapminder %>%
  distinct(continent)

# print the dataframe, unique_continents 
unique_continents 


# identify unique continents 
continents <- gapminder %>%
  distinct(continent) %>%
  pull(continent)

# display the vector
continents 


# identify the number of unique countries in the dataset
gapminder %>%
  distinct(country) %>%
  count()


# identify the top countries with the largest populations in 2015
gapminder_arranged1 <- gapminder %>% 
  filter(year == 2015) %>%
  arrange((country_population))


gapminder_arranged2 <- gapminder %>% 
  filter(year == 2015) %>%
  arrange(desc(country_population))


# identify the ten countries that had the highest life expectancy in 2015, and display their population
gapminder %>%
  filter(year == 2015) %>%
  arrange(desc(life_expectancy)) %>%
  select(country, life_expectancy, country_population) %>%
  head(10)


# add a new column, population_millions
gapminder_population <- gapminder %>% 
  mutate(population_millions = population / 1000000)

# add a new column, population_millions 
gapminder_population2 <- gapminder  %>% 
  mutate(population_millions = round(population / 1000000,2))


# extract the country name, year, life expectancy, continent, and region of countries and identify which countries are located in Asia or not
gapminder_is_asia <- gapminder %>%
  select(country, year, life_expectancy, continent, region) %>%
  mutate(is_asia = ifelse(continent == "Asia", 1, 0))

View(gapminder_is_asia)


# check levels attribute of the factor variable, continent
levels(gapminder$continent)


# count the number of observations in each continent
continent_count <- fct_count(gapminder$continent)

View(continent_count)


# sort the result by setting argument, sort to TRUE, and also display fraction by prop argument
continent_count_sorted <- fct_count(gapminder$continent, sort = TRUE, prop = TRUE)

View(continent_count_sorted)

#  add "Antarctica" to the continent variable
gapminder$continent <- fct_expand(gapminder$continent, "Antarctica")

# check levels of the continent to verify if Antarctica has been added or not
levels(gapminder$continent)


# count the number of observations of each continent
fct_count(gapminder$continent)

# drop unused levels from the continent variable
gapminder$continent <- fct_drop(gapminder$continent)


levels(gapminder$continent)

# move Asia to the second position
gapminder$continent <- fct_relevel(gapminder$continent, "Asia", after = 1)

levels(gapminder$continent)


# number of missing values in the column infant_mortality
sum(is.na(gapminder$infant_mortality))


# number of missing values in each column of gapminder dataset
colSums(is.na(gapminder))


# function to display the missing values summary
missing_values_df <- function(df) {
  
  # column wise missing values
  missing_values <- colSums(is.na(df))
  
  # percentage of missing values
  missing_values_percent <- 100 * colSums(is.na(df)) / nrow(df)
  
  # create a dataframe with the results
  missing_values_table <- cbind(missing_values , missing_values_percent)
  
  # rename the columns
  colnames(missing_values_table) <- c("Missing Values", "% of Total Values")
  
  # sort the table by percentage of missing descending
  missing_values_table <- missing_values_table[missing_values_table[,2] != 0,]
  missing_values_table <- missing_values_table[order(-missing_values_table[,2]),]
  missing_values_table <- round(missing_values_table, 1)
  
  # return the dataframe with missing values details
  return(missing_values_table)
}

gapminder_NA_summ <- missing_values_df(gapminder)

# print the result of gapminder_NA_summ 
gapminder_NA_summ 

# load the naniar package
library(naniar)


# locations of missing values of each variable
vis_miss(gapminder)



# number of missing values in each variable
gg_miss_var(gapminder)

# explore combinations of missingness among variables
gg_miss_upset(gapminder)


# check heatmap of missingness of variables along the years
gg_miss_fct(x= gapminder, fct = year)


# check the dimensions of gapminder
dim(gapminder)


# drop rows: missing values in any column
drop_rows_any <- gapminder %>% drop_na()

# check the dimensions of the new dataframe
dim(drop_rows_any)

# drop rows: missing values in any specific column ( for example, fertility)
drop_rows_fertility <- gapminder %>% drop_na(fertility)

# check the dimensions of the new dataframe
dim(drop_rows_fertility)


# drop the column country_gdp (using base R)
gapminder$country_gdp <- NULL

# drop the column country_gdp (using dplyr)
gapminder <- select(gapminder, -country_gdp)





