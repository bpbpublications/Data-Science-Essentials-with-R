
#install.packages("tidyverse")
#library(tidyverse)

library(dplyr)

install.packages("lubridate")
library(lubridate)


# create a Date Object from a String

ymd("2023-08-30")

ymd("20230830")

mdy("09 02 2023")

dmy("31-Aug-2023")

ymd_hms("2023-09-02 14:30:45")

mdy_hm("09/02/2023 14:30")

class(ymd("2023-08-30"))

class(ymd_hms("2023-09-02 14:30:45"))

# create a dataframe with individual date components

date_data <- data.frame(
  year = c(2023, 2022, 2021),
  month = c(8, 12, 5),
  day = c(24, 15, 3)
)


# add the new date column

date_data <- date_data %>%
  mutate(full_date = make_date(year, month, day))

# display the result
print(date_data)

str(date_data$full_date)

# defaults for missing elements

make_date(year = 2023, month = 9)   # day = 1

make_date(month = 9, day = 15)      # year = 1970

make_date(year = 2023, day = 15)    # month = 1


# sample data with individual date and time components

datetime_data <- data.frame(
  year = c(2023, 2022, 2021),
  month = c(8, 12, 5),
  day = c(24, 15, 3),
  hour = c(10, 14, 8),
  minute = c(30, 15, 45)
)

# add the new datetime column

datetime_data <- datetime_data %>%
  mutate(full_datetime = make_datetime(year, month, day, hour, minute))

# display the result
print(datetime_data)


# Create a timestamp for the morning run

run_timestamp <- ymd_hm("2023-08-25 06:30")

run_timestamp

# Identifies the year
year(run_timestamp)    

# Identifies the month
month(run_timestamp)   

# Identifies the day of the month 
mday(run_timestamp)   

# Identifies the day of the year 
yday(run_timestamp)   

# Identifies the day of the week (Friday, represented as 6)
wday(run_timestamp)   

# Setting label = TRUE displays the day of the week as an ordered factor
wday(run_timestamp, label = T)

# create a datetime object

event_datetime <- ymd_hms("2023-09-15 14:30:45")

# display the original datetime

event_datetime

# update multiple components: change to September 14, 2023, at 16:45:00

event_datetime <- update(
  event_datetime,
  day = 14,
  hour = 16,
  minute = 45,
  second = 00
)

# display the updated datetime

event_datetime

# read the CSV file
monthly_electricity  <- read.csv("MonthlyElectricityStatistics.csv")

# structure of the monthly_electricity dataframe
str(monthly_electricity)

# display the first 6 rows of the dataset
head(monthly_electricity)

# datatype of the column Time
class(monthly_electricity$Time)

# add a day component (e.g., 01) and parse the date

monthly_electricity <- monthly_electricity %>%
  mutate(
    Time = dmy(paste("01", Time))
  )


# extract different components from Time

monthly_electricity <- monthly_electricity %>%
  mutate(
    Month = month(Time),
    Year = year(Time),
    Quarter = quarter(Time, type = "year.quarter")
  )

head(monthly_electricity)

# subset the electricity_subset dataframe

electricity_subset <- monthly_electricity %>%
  filter(Country == "India", Product == "Electricity", Year >= 2021)

# summarise the electricity_subset dataset

summary_electricity <- electricity_subset %>%
  group_by(Quarter) %>%
  summarize(Total_Value = sum(Value),
            Average_Value = mean(Value))

# display the summary_electricity dataframe

View(summary_electricity)

# create previous_month_Value column

electricity_subset <- electricity_subset %>%
  select(Time, Value) %>%
  mutate(
    previous_month_Value = lead(Value)
  )

# calculate 'Value compared to previous month' and Percentage_Difference

electricity_subset <- electricity_subset %>%
  mutate(
    Value_Compared_To_Previous_Month = Value - previous_month_Value,
    Percentage_Difference = round(((Value - previous_month_Value) / previous_month_Value) * 100, 2)
  )

head(electricity_subset)

# downtime durations

downtime_durations <- c(duration(hours = 1), duration(minutes = 30), duration(hours = 2))

downtime_durations

class(downtime_durations)

# calculate total downtime

total_downtime <- sum(downtime_durations)

total_downtime

# subscription renewal periods

renewal_periods <- c(period(weeks = 2), period(weeks = 4), period(weeks = 6))

renewal_periods

class(renewal_periods)

# Sys.Date() returns the present day 
Sys.Date()

# calculate renewal dates
renewal_dates <- Sys.Date() + renewal_periods

print(renewal_dates)

# define event intervals

event_intervals <- c(interval(ymd_hms("2023-08-01 10:00:00"), ymd_hms("2023-08-01 12:00:00")), 
                     interval(ymd_hms("2023-08-03 15:30:00"), ymd_hms("2023-08-03 17:00:00")))

event_intervals

class(event_intervals)

# check if an event overlaps with a specific time

check_time <- ymd_hms("2023-08-01 11:30:00")

overlapping_events <- event_intervals[check_time %within% event_intervals]

print(overlapping_events)

