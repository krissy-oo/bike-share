---
title: "Cyclistic Bike Share"
author: "Christabel Okosun"
date: '2022-04-28'
output: html_document
editor_options: 
  chunk_output_type: console
---

Installing the required packages

```{r}
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
```

Loading the packages
```{r}
library("tidyverse")
library(lubridate)
library(ggplot2)
```

Importing datsets 
```{r}
library(readr)
bike_2019_q2 <- read_csv("C:/Users/user/Downloads/Cyclistic_Bike_Share/bike 2019 q2.csv")
bike_2019_q3<- read_csv("C:/Users/user/Downloads/Cyclistic_Bike_Share/bike 2019 q3.csv")
bike_2019_q4 <- read_csv("C:/Users/user/Downloads/Cyclistic_Bike_Share/Bike trips 2019 Q4.csv")
bike_2020_q1 <- read_csv("C:/Users/user/Downloads/Cyclistic_Bike_Share/bike trips 2020 q1.csv")
```
##Comparing Column Names
```{r}
colnames(bike_2019_q2)
colnames(bike_2019_q3)
colnames(bike_2019_q4)
colnames(bike_2020_q1)
```
##Renaming Column names
```{r}
(bike_2019_q2 <- rename(bike_2019_q2
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))
(bike_2019_q3 <- rename(bike_2019_q3
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))
(bike_2019_q4<- rename(bike_2019_q4
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))


```

##Inspecting dataframes
```{r}
str(bike_2019_q2)
str(bike_2019_q3)
str(bike_2019_q4)
str(bike_2020_q1)
```
##Converting ride_id and rideable_id to character so it can be stacked correctly
```{r}
bike_2019_q4 <-  mutate(bike_2019_q4, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
bike_2019_q3 <-  mutate(bike_2019_q3, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
bike_2019_q2 <-  mutate(bike_2019_q2, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
```
##Combining the dataframes
```{r}
Bike_trips<-bind_rows(bike_2019_q2,bike_2019_q3,bike_2019_q4,bike_2020_q1)
```
##Revoming Irrelevant columns
```{r}
Bike_trips<-Bike_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))
```
##Inspecting the new table created
```{r}
colnames(Bike_trips)
nrow(Bike_trips)
dim(Bike_trips)
head(Bike_trips)
```
##Statistical Summary of data
```{r}
summary(Bike_trips)
```
##Making the Member_casual column consistent
```{r}
table(Bike_trips$member_casual)
Bike_trips<-Bike_trips %>% 
  mutate(member_casual = recode(member_casual
                           ,"Subscriber" = "member"
                           ,"Customer" = "casual"))
```
###Adding columns to list the date, time and year
```{r}
Bike_trips$date <- as.Date(Bike_trips$started_at) #The default format is yyyy-mm-dd
Bike_trips$month <- format(as.Date(Bike_trips$date), "%m")
Bike_trips$day <- format(as.Date(Bike_trips$date), "%d")
Bike_trips$year <- format(as.Date(Bike_trips$date), "%Y")
Bike_trips$day_of_week <- format(as.Date(Bike_trips$date), "%A")
```

##Creating a calaculated field for ride_length
```{r}
Bike_trips$ride_length <- difftime(Bike_trips$ended_at,Bike_trips$started_at)
Bike_trips$ride_length <- as.numeric(as.character(Bike_trips$ride_length))
is.numeric(Bike_trips$ride_length)
```
##Revoming bad data and creating a new dataframe
```{r}
Bike_trips_01 <- Bike_trips[!(Bike_trips$start_station_name == "HQ QR" | Bike_trips$ride_length<0),]
```
##Conducting descriptive analysis
```{r}
summary(Bike_trips_01$ride_length)
```
##Comparaing members and casual users types
```{r}
aggregate(Bike_trips_01$ride_length ~ Bike_trips_01$member_casual, FUN = mean)
aggregate(Bike_trips_01$ride_length ~ Bike_trips_01$member_casual, FUN = median)
aggregate(Bike_trips_01$ride_length ~ Bike_trips_01$member_casual, FUN = max)
aggregate(Bike_trips_01$ride_length ~ Bike_trips_01$member_casual, FUN = min)
aggregate(Bike_trips_01$ride_length ~ Bike_trips_01$member_casual, FUN = mean)
```
##To order days of week
```{r}
Bike_trips_01$day_of_week <- ordered(Bike_trips_01$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

```

##Calculating the average ride time by each day for members and casual user types
```{r}
aggregate(Bike_trips_01$ride_length ~ Bike_trips_01$member_casual + Bike_trips_01$day_of_week, FUN = mean)
```
##Analyze ridership data by type and weekday
```{r}

Bike_trips_01 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()                            #calculates the number of rides and average duration
  ,average_duration = mean(ride_length)) %>%        #calculates the average duration
  arrange(member_casual, weekday)                               #sorts
```
##Visualing the number of rides by rider type
```{r}
 Bike_trips_01 %>% 
 mutate(weekday = wday(started_at, label = TRUE)) %>% 
group_by(member_casual, weekday) %>% 
summarise(number_of_rides = n()
 ,average_duration = mean(ride_length)) %>% 
arrange(member_casual, weekday)  %>% 
ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
geom_col(position = "dodge")
```
##AVerage duration
```{r}
Bike_trips_01 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
```
##Exporting Summary file
```{r}
counts <- aggregate(Bike_trips_01$ride_length ~ Bike_trips_01$member_casual + Bike_trips_01$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')
