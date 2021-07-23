---
                          title: ***"Cyclist Case Study"

author: **"Daniel Orozco"

date: "7/16/2021"
output: html_document

*Introduction
Cyclistic is a Chicago based company that provides a bike-share service in through the city. The company wants to analyze how annual members use the bikes differently to casual riders. The goal is to implement a marketing campaign targeting the casual riders and convince them to become members.

The data used in this project dates from May 2020 to April 2021 and has been made available by Motivate International Inc. under this https://www.divvybikes.com/data-license-agreement.
---
```{r}
library(tidyverse)
library(lubridate)
```
**Download and Store Cyclist monthly data
```{r}

# Use read_csv function to load the data

may_2020 <- read_csv("~/Desktop/Capstone Project/Cyclistic Case Study/Cyclist Data_csv/202005-divvy-tripdata.csv")
june_2020 <- read_csv("~/Desktop/Capstone Project/Cyclistic Case Study/Cyclist Data_csv/202006-divvy-tripdata.csv")
july_2020 <- read_csv("~/Desktop/Capstone Project/Cyclistic Case Study/Cyclist Data_csv/202007-divvy-tripdata.csv")
aug_2020 <- read_csv("~/Desktop/Capstone Project/Cyclistic Case Study/Cyclist Data_csv/202008-divvy-tripdata.csv")
sep_2020 <- read_csv("~/Desktop/Capstone Project/Cyclistic Case Study/Cyclist Data_csv/202009-divvy-tripdata.csv")
oct_2020 <- read_csv("~/Desktop/Capstone Project/Cyclistic Case Study/Cyclist Data_csv/202010-divvy-tripdata.csv")
nov_2020 <- read_csv("~/Desktop/Capstone Project/Cyclistic Case Study/Cyclist Data_csv/202011-divvy-tripdata.csv")
dec_2020 <- read_csv("~/Desktop/Capstone Project/Cyclistic Case Study/Cyclist Data_csv/202012-divvy-tripdata.csv")
jan_2021 <- read_csv("~/Desktop/Capstone Project/Cyclistic Case Study/Cyclist Data_csv/202101-divvy-tripdata.csv")
feb_2021 <- read_csv("~/Desktop/Capstone Project/Cyclistic Case Study/Cyclist Data_csv/202102-divvy-tripdata.csv")
mar_2021 <- read_csv("~/Desktop/Capstone Project/Cyclistic Case Study/Cyclist Data_csv/202103-divvy-tripdata.csv")
april_2021 <- read_csv("~/Desktop/Capstone Project/Cyclistic Case Study/Cyclist Data_csv/202104-divvy-tripdata.csv")


```

**Exploring the data
```{r}
# Learning the colum names of the data
colnames(may_2020)
colnames(june_2020)
colnames(july_2020)
colnames(aug_2020)
colnames(sep_2020)
colnames(oct_2020)
colnames(nov_2020)
colnames(dec_2020)
colnames(jan_2021)
colnames(feb_2021)
colnames(mar_2021)
colnames(april_2021)






```
**Exploring the structure of the data
```{r}
str(may_2020)
str(june_2020)
str(july_2020)
str(aug_2020)
str(sep_2020)
str(oct_2020)
str(nov_2020)
str(dec_2020) 
str(jan_2021)
str(feb_2021)
str(mar_2021)
str(april_2021) 
```
**Transforming and cleaning the data
```{r}
# Changing columns to the appropriate format in order to join all the data sets 


may_2020 <- mutate(may_2020, 
                   start_station_id = as.character(start_station_id)
                   ,end_station_id = as.character(start_station_id ))

june_2020<- mutate(june_2020, 
                   start_station_id = as.character(start_station_id)
                  ,end_station_id = as.character(start_station_id ))

july_2020 <- mutate(july_2020, 
                    start_station_id = as.character(start_station_id)
                   ,end_station_id = as.character(start_station_id ))

aug_2020 <- mutate(aug_2020, 
                   start_station_id = as.character(start_station_id)
                     ,end_station_id = as.character(start_station_id ))

sep_2020 <- mutate(sep_2020, 
                   start_station_id = as.character(start_station_id)
                   ,end_station_id = as.character(start_station_id ))


oct_2020 <- mutate(oct_2020, 
                   start_station_id = as.character(start_station_id)
                   ,end_station_id = as.character(start_station_id ))

nov_2020 <- mutate(nov_2020, start_station_id = as.character(start_station_id)
                   ,end_station_id = as.character(start_station_id ))

dec_2020 <- mutate(dec_2020, start_station_id = as.character(start_station_id)
                   ,end_station_id = as.character(start_station_id ))

jan_2021 <- mutate(jan_2021, start_station_id = as.character(start_station_id)
                   ,end_station_id = as.character(start_station_id ))

feb_2021 <- mutate(feb_2021, start_station_id = as.character(start_station_id)
                   ,end_station_id = as.character(start_station_id ))

mar_2021<- mutate(mar_2021, start_station_id = as.character(start_station_id)
                   ,end_station_id = as.character(start_station_id ))

april_2021 <- mutate(april_2021, start_station_id = as.character(start_station_id)
                   ,end_station_id = as.character(start_station_id ))

# Combining the data sets together

all_trips <- bind_rows(may_2020,june_2020,july_2020,aug_2020,sep_2020,oct_2020,nov_2020,dec_2020,jan_2021,feb_2021,mar_2021,april_2021)

# Check the structure of the created data frame

str(all_trips)

# Eliminate non usable columns
all_trips_ <- all_trips %>% 
  select(-c(start_lng,start_lat, end_lat, end_lng))


# Eliminate NA's from the data 

all_trips <- na.omit(all_trips)


glimpse(all_trips)

# Changing column names
all_trips <- all_trips %>%
  rename(ride_type = rideable_type, 
         customer_type = member_casual)
glimpse(all_trips)
table(all_trips$customer_type)

# Adding columns year,month and day 
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date),'%b_%y')
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")



# Finding the length of each ride in minutes
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at, units = "mins")
str(all_trips)


# Convert "ride_length" from Factor to numeric to run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

```
**Create a new version of the data frame (v2)
```{r}
# Remove "bad" data
# Create a new version of the data frame (v2) since data is being removed

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]



```
**Analyze 
```{r}

# Inspecting ride length
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# Average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$customer_type, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$customer_type, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$customer_type, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$customer_type, FUN = min)


# The functions summary provide a statistical summary of ride length for all trips
summary(all_trips_v2$ride_length)

# Correcting the month order according to the data 
all_trips_v2$month <- ordered(all_trips_v2$month, 
                              levels = c("May_20", "June_20",
                                          "Jul_20", "Aug_20","Sep_20","Oct_20",
                                          "Nov_20", "Dec_20", "Jan_21",
                                          "Feb_21", "Mar_21","Apr_21"))

# Descriptive analysis on ride length
all_trips_v2 %>% 
  summarise(min_ride_length = min(ride_length), 
            max_ride_length = max(ride_length),
            median_ride_length = median(ride_length),
            mean_ride_length = mean(ride_length))

# See the average ride length by month for members vs casual riders
aggregate(all_trips_v2$ride_length ~ all_trips_v2$customer_type + all_trips_v2$month, FUN = mean)


# Descriptive analysis on ride length
all_trips_v2 %>% 
  summarise(min_ride_length = min(ride_length), 
            max_ride_length = max(ride_length),
            median_ride_length = median(ride_length),
            mean_ride_length = mean(ride_length))

# Analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(customer_type, weekday) %>%  #groups by user type and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(customer_type, weekday)		

# Number of rides between members and casual riders for each month
all_trips_v2 %>% 
  group_by(customer_type, month) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(month)



```
**Visualization 
1- The number of rides by customer type vs day of the week
```{r}


all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(customer_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(customer_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = customer_type)) +
  geom_col(position = "dodge") +
   labs(title ="Trips by costumer type per day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
```
2- The average duration of ride by day of the week
```{r}

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(customer_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(customer_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = customer_type)) +
  geom_col(position = "dodge") +
  labs(title = "Average trip duration by customer type per day of the week")
```

3- Monthly rides by customer type
```{r}
all_trips_v2 %>% 
  group_by(customer_type, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(month)  %>% 
  drop_na %>% 
   ggplot(aes(x = month, y = number_of_rides, fill = customer_type)) +
  labs(title ="Number of rides by customer type per month") +
  theme(axis.text.x = element_text(angle = 65)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

```


4- Average duration of rides by month
```{r}
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(month, customer_type) %>%
  drop_na() %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(month, customer_type)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = customer_type)) +
  geom_col(position = "dodge")+
  labs(title = "Average trip duration per customer type by month")
```
```
** Findings

- During the period of time in question (april_2020-may_2021) casual riders average almost twice the ride time than the members. 
- Casual users use the bikes more during the weekends, while members have a more consistant use in the weekdays.
- The average ride time for members is less than 20 minutes. This indicates that members Tipically use the bikes to commute local areas or daily activities (e.g., work, school).
- The number of rides per month subtantionally decreases from Octouber 2020 to Febrerary 2021. Since the city in question is Chicago it can be assumed that this change is due to changing in seasons.


** Recomendations

1 Provide a promotional joining incentive for already casual users
2 Develop a marketing campaing promoting a tier membership based usage level. For example:
  * A limited number of rides for entry level users
  * A moderate number of rides for medium level riders
  * Continue unlimited 45 minutes rides for regular members
3 Offer seasonal memberships to already casual members icentivate the use in seasons of most demand.

