# setup working directory
setwd("C:/Divvy_Trips_2020/")

# install required packages
library(tidyverse)
library(lubridate)

# upload Divvy csv files into R as a tibble
q1_2020 <-read_csv("Divvy_Trips_2020_Q1.csv")
m4_2020 <-read_csv("Divvy_Trips_2020_04.csv")
m5_2020<-read_csv("Divvy_Trips_2020_05.csv")
m6_2020 <-read_csv("Divvy_Trips_2020_06.csv")
m7_2020 <-read_csv("Divvy_Trips_2020_07.csv")
m8_2020 <-read_csv("Divvy_Trips_2020_08.csv")
m9_2020 <-read_csv("Divvy_Trips_2020_09.csv")
m10_2020 <-read_csv("Divvy_Trips_2020_10.csv")
m11_2020 <-read_csv("Divvy_Trips_2020_11.csv")
m12_2020 <-read_csv("Divvy_Trips_2020_12.csv")

# Compare and find matching column names of each files
colnames(q1_2020)
colnames(m4_2020)
colnames(m5_2020)
colnames(m6_2020)
colnames(m7_2020)
colnames(m8_2020)
colnames(m9_2020)
colnames(m10_2020)
colnames(m11_2020)
colnames(m12_2020)


# Inspect the data frames for discrepancy
str(q1_2020)
str(m4_2020)
str(m5_2020)
str(m6_2020)
str(m7_2020)
str(m8_2020)
str(m9_2020)
str(m10_2020)
str(m11_2020)
str(m12_2020)

# Convert start_station_id and end_station_id to character
q1_2020 <- mutate(q1_2020, "start_station_id"= as.character("start_station_id")
                  ,"end_station_id" = as.character("end_station_id"))

m4_2020 <- mutate(m4_2020, "start_station_id"= as.character("start_station_id")
                  ,"end_station_id" = as.character("end_station_id"))

m5_2020 <- mutate(m5_2020, "start_station_id"= as.character("start_station_id")
                  ,"end_station_id" = as.character("end_station_id"))

m6_2020 <- mutate(m6_2020, "start_station_id"= as.character("start_station_id")
                  ,"end_station_id" = as.character("end_station_id"))

m7_2020 <- mutate(m7_2020, "start_station_id"= as.character("start_station_id")
                  ,"end_station_id" = as.character("end_station_id"))

m8_2020 <- mutate(m8_2020, "start_station_id"= as.character("start_station_id")
                  ,"end_station_id" = as.character("end_station_id"))

m9_2020 <- mutate(m9_2020, "start_station_id"= as.character("start_station_id")
                  ,"end_station_id" = as.character("end_station_id"))

m10_2020 <- mutate(m10_2020, "start_station_id"= as.character("start_station_id")
                  ,"end_station_id" = as.character("end_station_id"))

m11_2020 <- mutate(m11_2020, "start_station_id"= as.character("start_station_id")
                  ,"end_station_id" = as.character("end_station_id"))



# Merge all data frames into one large data frame
all_trips <- bind_rows(q1_2020, m4_2020, m5_2020, m6_2020, m7_2020, m8_2020, m9_2020, m10_2020, m11_2020, m12_2020)

# Some of NA's value in start_station_id and end_station_id were replace with non NA values in this case their col names after the merge operation; change them back to NA's

all_trips %>% mutate(start_station_id = replace(start_station_id, start_station_id =="start_station_id", NA))

all_trips %>% mutate(end_station_id = replace(end_station_id, end_station_id =="end_station_id", NA))

# Remove lat and long from the data frame
all_trips <- all_trips %>% select(-c(start_lat, start_lng, end_lat, end_lng))

# Inspect the new column created
# List of column names
colnames(all_trips)

# How many rows in the data frame
# 3,541,683 rows
nrow(all_trips)

# Dimension of the data frame
# 3,541,683 rows, 9 columns
dim(all_trips)

# Data frame structure
 str(all_trips)
 
 # Statistic Summary of data
 summary(all_trips)

 # Add more fields: day, month, year for data aggregation
 # Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level
 # Check member_casual column
 table(all_trips$member_casual)
 
 # https://www.statmethods.net/input/
 # The default format is yyyy-mm-dd
 all_trips$date <-as.Date(all_trips$started_at)
 all_trips$month <-format(as.Date(all_trips$date), "%m")
 all_trips$day <-format(as.Date(all_trips$date), "%d")
 all_trips$year <-format(as.Date(all_trips$date), "%Y")
 all_trips$day_of_week <-format(as.Date(all_trips$date), "%A")
 
 # Add a "ride_length" calculation to all_trips(in seconds)
 # https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
 all_trips$ride_length <- difftime(all_trips$ended_at ,all_trips$started_at)
 
 # New Data Frame Structure
 str(all_trips)
 
 # Convert "ride_length" from Factor to numeric so we can run calculations on the data
 is.factor(all_trips$ride_length)
 all_trips$ride_length <-as.numeric(as.character(all_trips$ride_length))
 is.numeric(all_trips$ride_length)
 
 # Remove "bad" data (negative values) and create a new data frame.
 # # https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
 all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0),]
 
 # Statistic Summary of ride length

 summary(all_trips_v2$ride_length)
 
 # sort the data frame and look for NA
all_trips_v2 %>% arrange(!is.na(ride_id), ride_id)
 
 # Our summary analysis shows 94609 NA's. Remove all the rows with NA using column ride_id or ride_length and create a new data frame: all_trips_v3
 
 all_trips_v3 <- all_trips_v2[complete.cases(all_trips_v2$ride_id),]
 
 # Repeat the statistical summary of ride length
 summary(all_trips_v3$ride_length)
 
 # Compare member and casual user
 aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = mean)
 aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = median)
 aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = max)
 aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = min)
 
 # See the average ride time by each day for member and casual user
 aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)
 

# Fix the days of the week, because they are out of order
 all_trips_v3$day_of_week <- ordered(all_trips_v3$day_of_week, levels=c("Sunday", "Monday",
                                                                        "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
 
 # Run average ride time by each day for member and casual user
 aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)
 
 
 # Analyze ridership data by type and weekday; create weekday field using wday(); group by usertype and weekday 
 # Calculate the number of rides and average calculates; calculate the average duration
 # sorts
 all_trips_v3 %>% mutate(weekday = wday(started_at, label = TRUE)) %>% group_by(member_casual,weekday) %>% summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
         arrange(member_casual, weekday)
 
 # Let's visualize the number of rides by rider type
 all_trips_v3 %>% mutate(weekday = wday(started_at, label = TRUE)) %>% group_by(member_casual, weekday) %>%
         summarise(number_of_rides = n()
         ,average_duration = mean(ride_length)) %>%
         arrange(member_casual, weekday) %>%
         ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
         geom_col(position = "dodge")
 
 # Let's visualize the average duration of all trips
 all_trips_v3 %>%
         mutate(weekday = wday(started_at, label = TRUE)) %>%
         group_by(member_casual, weekday) %>%
         summarise(number_of_rides = n()
                   ,average_duration = mean(ride_length)) %>%
         arrange(member_casual, weekday) %>%
         ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
         geom_col(position = "dodge")
 
 # visualize bike preference between rider type
 
 ggplot(data=all_trips_2020) +geom_bar(mapping = aes(x=rideable_type, fill = member_casual), position = "dodge") + ggtitle("Bike Preferences Among Riders type, 2020") + theme(plot.title = element_text(hjust = 0.5))
 
 # average duration of all trips by months
 all_trips_2020 %>%
         mutate(month = month(started_at, label = TRUE)) %>%
         group_by(member_casual, month) %>%
         summarise(number_of_rides = n()
                   ,average_duration = mean(ride_length)) %>%
         arrange(member_casual, month) %>%
         ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
         geom_col(position = "dodge") ("The Average Duration of All Trips per Month, 2020") + theme(plot.title = element_text(hjust = 0.5))
 
# number of rides by month
  all_trips_2020 %>% mutate(month = month(started_at, label = TRUE)) %>% group_by(member_casual, month) %>%
         summarise(number_of_rides = n()
                   ,average_duration = mean(ride_length)) %>%
         arrange(member_casual, month) %>%
         ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
         geom_col(position = "dodge") +ggtitle("The Number of Rides per Month By Rider Type, 2020") + theme(plot.title = element_text(hjust = 0.5))
 
 # Create a csv file for further analysis
 # https://datatofish.com/export-dataframe-to-csv-in-r/ for furthe information in export
 counts <- aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual +
                             all_trips_v3$day_of_week, FUN = mean)
 write.csv(counts,file ="C:/Divvy_Trips_2020/avg_ride_length.csv")
 write.csv(all_trips_v3, file ="C:/Divvy_Trips_2020/all_trips_2020.csv", row.names = FALSE)
 
 
