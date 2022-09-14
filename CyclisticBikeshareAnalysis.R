# installing and loading necessary R Packages
install.packages("tidyverse")
library(tidyverse)

# importing the csv files into data frames
df_202207 <- read.csv("C:\\Users\\KH268LL\\OneDrive - EY\\Documents\\Google Data Analytics Certificate\\Course 8\\Case Study 1 Data sources\\202207-divvy-tripdata.csv")
df_202206 <- read.csv("C:\\Users\\KH268LL\\OneDrive - EY\\Documents\\Google Data Analytics Certificate\\Course 8\\Case Study 1 Data sources\\202206-divvy-tripdata.csv")
df_202205 <- read.csv("C:\\Users\\KH268LL\\OneDrive - EY\\Documents\\Google Data Analytics Certificate\\Course 8\\Case Study 1 Data sources\\202205-divvy-tripdata.csv")
df_202204 <- read.csv("C:\\Users\\KH268LL\\OneDrive - EY\\Documents\\Google Data Analytics Certificate\\Course 8\\Case Study 1 Data sources\\202204-divvy-tripdata.csv")
df_202203 <- read.csv("C:\\Users\\KH268LL\\OneDrive - EY\\Documents\\Google Data Analytics Certificate\\Course 8\\Case Study 1 Data sources\\202203-divvy-tripdata.csv")
df_202202 <- read.csv("C:\\Users\\KH268LL\\OneDrive - EY\\Documents\\Google Data Analytics Certificate\\Course 8\\Case Study 1 Data sources\\202202-divvy-tripdata.csv")
df_202201 <- read.csv("C:\\Users\\KH268LL\\OneDrive - EY\\Documents\\Google Data Analytics Certificate\\Course 8\\Case Study 1 Data sources\\202201-divvy-tripdata.csv")
df_202112 <- read.csv("C:\\Users\\KH268LL\\OneDrive - EY\\Documents\\Google Data Analytics Certificate\\Course 8\\Case Study 1 Data sources\\202112-divvy-tripdata.csv")
df_202111 <- read.csv("C:\\Users\\KH268LL\\OneDrive - EY\\Documents\\Google Data Analytics Certificate\\Course 8\\Case Study 1 Data sources\\202111-divvy-tripdata.csv")
df_202110 <- read.csv("C:\\Users\\KH268LL\\OneDrive - EY\\Documents\\Google Data Analytics Certificate\\Course 8\\Case Study 1 Data sources\\202110-divvy-tripdata.csv")
df_202109 <- read.csv("C:\\Users\\KH268LL\\OneDrive - EY\\Documents\\Google Data Analytics Certificate\\Course 8\\Case Study 1 Data sources\\202109-divvy-tripdata.csv")
df_202108 <- read.csv("C:\\Users\\KH268LL\\OneDrive - EY\\Documents\\Google Data Analytics Certificate\\Course 8\\Case Study 1 Data sources\\202108-divvy-tripdata.csv")

# Appending all the individual dataframes into single dataframe by combining do.call() with rbind(), since we need to append multiple dataframes. rbind() appends 1 dataframe to another dataframes. 
bikeshare_merged_df <- do.call("rbind", list(df_202108,df_202109,df_202110,df_202111,df_202112,df_202201,df_202202,df_202203,df_202204,df_202205,df_202206,df_202207))
# number of rows in the merged dataframe - 5901463
#nrow(bikeshare_merged_df)

# converting empty strings into NA values and removing all rows containing NA values in the dataframe - 4629230

bikeshare_no_na_df <- na_if(bikeshare_merged_df, "") %>%
  na.omit()
#nrow(bikeshare_no_na_df)

# extracting dates from started_at and ended_at columns
bikeshare_date_time_df <- bikeshare_no_na_df %>%
  mutate(start_date = as.Date(started_at), end_date = as.Date(ended_at))
#View(bikeshare_date_time_df)

#calculating ride length
#library(hms)
#bikeshare_date_time_df %>%
#  mutate(ride_length = as_hms(difftime(ended_at,started_at)))

# creating ride length and day of week column - 1 being Sunday and 7 being Monday
library(lubridate)
library(hms)
bikeshare_df <- bikeshare_date_time_df %>%
  mutate(ride_length = as_hms(difftime(ended_at,started_at)),day_of_week = wday(started_at))
View(bikeshare_df)

# exporting cleansed dataframe to csv file
#write_csv(bikeshare_df,"C:\\Users\\KH268LL\\OneDrive - EY\\Documents\\Google Data Analytics Certificate\\Course 8\\bike_share.csv")

# DESCRIPTIVE ANALYSIS

# column names in the dataframe
colnames(bikeshare_df)

# number of rows in the dataframe
nrow(bikeshare_df)

# structure of the dataframe
str(bikeshare_df)

# statistical summary about the dataframe
summary(bikeshare_df)

# first 6 rows of the dataframe
head(bikeshare_df)

# last 6 rows of the dataframe
tail(bikeshare_df)

# ride length summary
bikeshare_df %>%
  summarise(average_ride_length <- mean(ride_length), minimum_ride_length <- min(ride_length), maximum_ride_length <- max(ride_length))

# most frequently occuring day of week in the dataset
bikeshare_df %>%
 summarise(mode(day_of_week))

# ANALYZING TRENDS IN MEMBERS AND CASUAL RIDERS

# average ride duration and number of rides comparison
bikeshare_df %>%
  group_by(member_casual) %>%
  summarise(average_duration=mean(ride_length), number_of_rides=n())

# average ride duration by day of week comparison
bikeshare_df %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_ride_length = mean(ride_length))

# number of rides by day of week comparison
bikeshare_df %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, day_of_week)

# frequency of bike types used comparison
bikeshare_df %>%
  group_by(member_casual, rideable_type) %>%
  summarise(n())

# rides each year comparison
bikeshare_df %>%
  group_by(member_casual, year(start_date)) %>%
  summarise(rides_per_year=n())

# rides each month comparison
bikeshare_df %>%
  group_by(member_casual, month(start_date)) %>%
  summarise(rides_per_year=n())

View(bikeshare_df)