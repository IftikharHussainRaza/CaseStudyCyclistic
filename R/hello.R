


# Google Data Analytics, Case Study 1, Cyclistic Bike Sharing Company

# Tidyverse package is already installed, installing tidyverse laibrary

library(tidyverse)

# Upload divvy datasets (csv files)
df2210 <- read_csv("E:/R/trips_data/202210-divvy-tripdata.csv")
df2211 <- read_csv("E:/R/trips_data/202211-divvy-tripdata.csv")
df2212 <- read_csv("E:/R/trips_data/202212-divvy-tripdata.csv")
df2301 <- read_csv("E:/R/trips_data/202301-divvy-tripdata.csv")
df2302 <- read_csv("E:/R/trips_data/202302-divvy-tripdata.csv")
df2303 <- read_csv("E:/R/trips_data/202303-divvy-tripdata.csv")
df2304 <- read_csv("E:/R/trips_data/202304-divvy-tripdata.csv")
df2305 <- read_csv("E:/R/trips_data/202305-divvy-tripdata.csv")
df2306 <- read_csv("E:/R/trips_data/202306-divvy-tripdata.csv")
df2307 <- read_csv("E:/R/trips_data/202307-divvy-tripdata.csv")
df2308 <- read_csv("E:/R/trips_data/202308-divvy-tripdata.csv")
df2309 <- read_csv("E:/R/trips_data/202309-divvy-tripdata.csv")


# Compare column names before merging all the data
col_names <- cbind(colnames(df2210),
                   colnames(df2211),
                   colnames(df2212),
                   colnames(df2301),
                   colnames(df2302),
                   colnames(df2303),
                   colnames(df2304),
                   colnames(df2305),
                   colnames(df2306),
                   colnames(df2307),
                   colnames(df2308),
                   colnames(df2309))

# Display the names to compare
knitr::kable(col_names)

# All columns are same, therefore merging all months' files
all_trips <- bind_rows(df2210,
                       df2211,
                       df2212,
                       df2301,
                       df2302,
                       df2303,
                       df2304,
                       df2305,
                       df2306,
                       df2307,
                       df2308,
                       df2309)

# Inspect summary of combined data
summary(all_trips)

# Our analysis depends upon annual members and casual riders,
# therefore, checking if all the entries are "member" and "casual" only
unique(all_trips$member_casual)

# Add columns that list the hour, date, month, day, and year of each ride
# This will allow us to aggregate ride data for each hour, month, day, or
# year, before completing these operations we can only aggregate
# at the ride level

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(all_trips$date, "%m")
all_trips$day <- format(all_trips$date, "%d")
all_trips$year <- format(all_trips$date, "%Y")
all_trips$day_of_week <- format(all_trips$date, "%A")
all_trips$hour <- format(all_trips$started_at, "%H")

# Now calculate ride_length for each ride in a new column
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)

# There are some negative and some greater than a day values in ride_length, seems some error
# therefore, eliminating such values. Minimum time required to nearest station is 300 seconds
# therefore, taking minimum ride_length as 300 seconds
all_trips_v2 <- subset(all_trips, ride_length > 299 & ride_length <86400)

# Check if station id is missing
sum(is.na(all_trips_v2$start_station_name))
sum(is.na(all_trips_v2$end_station_name))

# Eliminate those rows where station name value is null. We will analyse the busiest stations
all_trips_v3 <- all_trips_v2 %>%
  drop_na(start_station_name)

all_trips_v3 <- all_trips_v2 %>%
  drop_na(end_station_name)


# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# To See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


# Noticed that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week,
                                    levels=c("Sunday", "Monday", "Tuesday", "Wednesday",
                                             "Thursday", "Friday", "Saturday"))


# Let's check if days issue is solved
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
# creates weekday field using wday()
# groups by user type and weekday
# calculates the number of rides and average duration
# calculates the average duration
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n() ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)

# Let's visualize the number of rides by rider type
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)  %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)  %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

all_trips_v2 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, month)  %>%
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

all_trips_v2 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, month)  %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Group the data by member_casual and start_station_name, and count the number of rides
# to identify the stations where casual riders prefer bike ride
all_trips_v3_grouped <- all_trips_v3 %>%
  group_by(member_casual, start_station_name) %>%
  summarise(num_of_rides = n())

# Filter the data to only keep the top 10 stations with the highest number of rides
df_top10 <- all_trips_v3_grouped %>%
  top_n(10, num_of_rides)

# Graph the data using a bar plot, with member_casual as the fill color
ggplot(df_top10, aes(x = start_station_name, y = num_of_rides, fill = "casual")) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Start Station Name", y = "Number of Rides", title = "Number of Rides from Start Station")


# Group the data by member_casual and end_station_name, and count the number of rides
# to identify the stations where casual riders prefer to bike ride


all_trips_v3_grouped <- all_trips_v3 %>%
  group_by(member_casual, end_station_name) %>%
  summarise(num_of_rides = n())

# Filter the data to only keep the top 10 stations with the highest number of rides
df_top10 <- all_trips_v3_grouped %>%
  top_n(10, num_of_rides)

# Graph the data using a bar plot, with member_casual as the fill color
ggplot(df_top10, aes(x = end_station_name, y = num_of_rides, fill = "casual")) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "End Station Name", y = "Number of Rides", title = "Number of Rides To End Station")







