# Google-Data-Analytics-Case-Study
I developed portfolio(How does a bike share navigate Speedy Success?) using R programming. To see portfolio visit : file:///C:/Users/hp/Documents/Case%20Study/Cycle.html
---
title: "How does a bike share navigate Speedy Success?"
author: "Praaiya"
date: "2023-08-26"
output: html_document
---

```{r echo=FALSE, out.width="100%" }

knitr::include_graphics("Cycle.jpg", error=FALSE)

```

**About the company**

In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geo tracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime. Until now, Cyclistic’s marketing strategy relied on building general awareness and appealing to broad consumer segments. One approach that helped make these things possible was the flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships. Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members.
               Cyclistic’s finance analysts have concluded that annual members are much more profitable than casual riders. Although the pricing flexibility helps Cyclistic attract more customers, The director of marketing believes that maximizing the number of annual members will be key to future growth. Rather than creating a marketing campaign that targets all-new customers, she believes there is a very good chance to convert casual riders into members. She notes that casual riders are already aware of the Cyclistic program and have chosen Cyclistic for their mobility needs.

          
**Ask**

The questions that need to be answered are:

·      How do annual members and casual riders use Cyclistic bikes differently?

·      Why would casual riders buy Cyclistic annual memberships?

·      How can Cyclistic use digital media to influence casual riders to become members?

**Prepare**

·      Check the Data Type

·      Check ROCCC ( Reliable, Original, Comprehensive, Current & Cited)

I will use Divya’s , bike-share program based in Chicago, data from Aug 2022- Aug 2023 to complete this case study. To download the data, please use[ this link](https://divvy-tripdata.s3.amazonaws.com/index.html) . This data was made public by Motivate International Inc, under this license.
```{r}
library(tidyverse)# Helps wrangle data
library(lubridate)# helps wrangle data attributes
library(ggplot2)# helps visualize data
library(dplyr)# helps clean data
library(tidyr)# helps clean data

```
```{r}
# Read th trip data from Aug 2022 to Aug 2023
library(readr)
tripdata_Aug22 <- read.csv("202207-divvy-tripdata.csv")
tripdata_Sep22 <- read.csv("202208-divvy-tripdata.csv")
tripdata_Oct22 <- read.csv("202209-divvy-publictripdata.csv")
tripdata_Nov22 <- read.csv("202210-divvy-tripdata.csv")
tripdata_Dec22 <- read.csv("202211-divvy-tripdata.csv")
tripdata_Jan23 <- read.csv("202212-divvy-tripdata.csv")
tripdata_Feb23 <- read.csv("202301-divvy-tripdata.csv")
tripdata_Mar23 <- read.csv("202302-divvy-tripdata.csv")
tripdata_Arp23 <- read.csv("202303-divvy-tripdata.csv")
tripdata_may23 <- read.csv("202304-divvy-tripdata.csv")
tripdata_Jun23 <- read.csv("202305-divvy-tripdata.csv")
tripdata_Jul23 <- read.csv("202306-divvy-tripdata.csv")
tripdata_Aug23 <- read.csv("202307-divvy-tripdata.csv")


```
```{r}
#Data Check
colnames(tripdata_Aug22)
colnames(tripdata_Sep22)
colnames(tripdata_Oct22)
colnames(tripdata_Nov22)
colnames(tripdata_Dec22)
colnames(tripdata_Jan23)
colnames(tripdata_Feb23)
colnames(tripdata_Mar23)
colnames(tripdata_Arp23)
colnames(tripdata_may23)
colnames(tripdata_Jun23)
colnames(tripdata_Jul23)
colnames(tripdata_Aug23)
#Confirmed none of column name should be changed

```
```{r}
#DATA CHECK
str(tripdata_Aug22)
str(tripdata_Sep22)
str(tripdata_Oct22)
str(tripdata_Nov22)
str(tripdata_Dec22)
str(tripdata_Jan23)
str(tripdata_Feb23)
str(tripdata_Mar23)
str(tripdata_Arp23)
str(tripdata_may23)
str(tripdata_Jun23)
str(tripdata_Jul23)
str(tripdata_Aug23)
#Confirmed
```


```{r}
#Convert some data type (from Double to Character) to merge
tripdata_Aug22 <- mutate(tripdata_Aug22, start_station_id= as.character(start_station_id), end_station_id=as.character(end_station_id))
tripdata_Sep22 <- mutate(tripdata_Sep22, start_station_id= as.character(start_station_id), end_station_id=as.character(end_station_id))
tripdata_Oct22 <- mutate(tripdata_Oct22, start_station_id= as.character(start_station_id), end_station_id=as.character(end_station_id))
tripdata_Nov22 <- mutate(tripdata_Nov22, start_station_id= as.character(start_station_id), end_station_id=as.character(end_station_id))
tripdata_Dec22 <- mutate(tripdata_Dec22, start_station_id= as.character(start_station_id), end_station_id=as.character(end_station_id))
tripdata_Jan23 <- mutate(tripdata_Jan23, start_station_id= as.character(start_station_id), end_station_id=as.character(end_station_id))
tripdata_Feb23 <- mutate(tripdata_Feb23, start_station_id= as.character(start_station_id), end_station_id=as.character(end_station_id))
tripdata_Mar23 <- mutate(tripdata_Mar23, start_station_id= as.character(start_station_id), end_station_id=as.character(end_station_id))
tripdata_Arp23 <- mutate(tripdata_Arp23, start_station_id= as.character(start_station_id), end_station_id=as.character(end_station_id))
tripdata_may23 <- mutate(tripdata_may23, start_station_id= as.character(start_station_id), end_station_id=as.character(end_station_id))
tripdata_Jun23 <- mutate(tripdata_Jun23, start_station_id= as.character(start_station_id), end_station_id=as.character(end_station_id))
tripdata_Jul23 <- mutate(tripdata_Jul23, start_station_id= as.character(start_station_id), end_station_id=as.character(end_station_id))
tripdata_Aug23 <- mutate(tripdata_Aug23, start_station_id= as.character(start_station_id), end_station_id=as.character(end_station_id))

```
```{r}
#Combine all the data sets
All_trips<- bind_rows(tripdata_Aug22,tripdata_Sep22,tripdata_Oct22,tripdata_Nov22,tripdata_Dec22,tripdata_Jan23,tripdata_Feb23,tripdata_Mar23,tripdata_Arp23,tripdata_may23,tripdata_Jun23,tripdata_Jul23,tripdata_Aug23)
```

**Process**

I have done Data Cleaning using R due to large data set.

```{r}
#List of all column names
colnames(All_trips)
```

```{r}
#How many rows are in data frames?
nrow(All_trips)
```
```{r}
#Dimension of the data frame
dim(All_trips)
```
```{r}
#First 6 rows of data frame
head(All_trips)
```
```{r}
#See list of columns and data types(numeric, character,etc)
str(All_trips)

```
```{r}
#Statistical summary of data. Mainly for numerics.
summary(All_trips)
```
Confirmed there are 7049 NA columns in end_lat and end_lng.

```{r}
#Add columns that list the date, month,day and year of each ride as we might need to aggregate ride data for each month, day or year.
All_trips$date<- as.Date(All_trips$started_at)# The default format is YYYY-MM-DD
All_trips$month<-format(as.Date(All_trips$date),"%m")
All_trips$day <-format(as.Date(All_trips$date),"%d")
All_trips$year<-format(as.Date(All_trips$date),"%Y")
All_trips$day_of_week<-format(as.Date(All_trips$date),"%A")
```


Let's check if additional columns have been added.

```{r}
colnames(All_trips)#Check additional column added
```
Confirmed additional columns have been added

Calculate ride_length to All_trips so that I can compare ride length for each ride.

```{r}
All_trips$ride_length <-difftime(All_trips$ended_at,All_trips$started_at)
```
```{r}
#Convert" "ride_length" from double to numeric so we can run calculations on the data
All_trips$ride_length <- as.numeric(as.character(All_trips$ride_length))
is.numeric(All_trips$ride_length)
```
*Remove Bad data*

The dataframe includes a few hundred entries when bikers were taken out of docks and checked for quality by Divvy or ride_length was negative. Now, We have to remove ride length is less than 0 second and is > 1440 minutes as ride length shouldn't be either negative or more than one day.

```{r}
All_trips_v2 <- All_trips[!(All_trips$ride_length <= 0 | All_trips$ride_length > 1440),]
```
```{r}
#Check new dataframe
dim(All_trips_v2)
View(All_trips_v2)
summary(All_trips_v2)
```
Confirmed the total number of rows 5545660. 187 Na's remains in end_lat and end_lng.

Remove NA data from the All_trips_v2 to get accurate data

```{r}
All_trips_v2 <- drop_na(All_trips_v2)#Drop all NAs
dim(All_trips_v2)
summary(All_trips_v2)
```
Remove duplicate ID as I confirmed ride ID is associate with each rides. It confirmed different ride_id is assigned for every rides even if same r ider uses uses this service

```{r}
All_trips_v3<-All_trips_v2[!duplicated(All_trips_v2$ride_id),]
dim(All_trips_v3)
```
Data Cleaning/Validation process has been completed at this point.I have created one additional columns for further analysis i.e Distance for each ride


```{r}
library(geosphere)
```
```{r}
start_matrix= matrix(c(All_trips_v3$start_lng, All_trips_v3$start_lat), ncol=2)
end_matrix= matrix(c(All_trips_v3$end_lng, All_trips_v3$end_lat), ncol=2)
```


```{r}
#Find out the distance for each ride
All_trips_v3$ride_distance<- distGeo(start_matrix,end_matrix)
View(All_trips_v3)
```

```{r}
summary(All_trips_v3)
```
**Analysis**

Find out the number of ride by type of rider.

```{r}
All_trips_v3$day_of_week <- replace(All_trips_v3$day_of_week,NA,"Sunday")
```

```{r}
#Assign the correct order to each day of the week
All_trips_v3$day_of_week <- ordered(All_trips_v3$day_of_week, levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
```

```{r}
All_trips_v3 %>%
  group_by(member_casual,day_of_week) %>%
  summarize(number_of_ride =n(), .groups = 'drop') %>%
  arrange(day_of_week)
```

```{r}
# Assign the correct order to each month of the year
All_trips_v3$month <- ordered(All_trips_v3$month, levels=c('08','09','10','11','12','01','02','03','04','05','06','07'))
```

```{r}
All_trips_v3 %>%
  group_by(member_casual,month) %>%
  summarize(number_of_rides =n(), .groups ='drop') %>%
  arrange(month)
```

*Finding*

1.Casual riders are more likely to take a ride on weekend while membership riders use on weekday     more often.
2. Summer is the peak season for both riders types.


Now,I would like to find out whether ride_length can be different depends on rider types


```{r}
aggregate(All_trips_v3$ride_length ~ All_trips_v3$member_casual + All_trips_v3$day_of_week,FUN=mean)
```

```{r}
All_trips_v3 %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_length = mean(ride_length), .groups = 'drop') %>%
  arrange(month)
```


*Finding*

1.Casual rider's trip is longer than membership ones regardless of the season or day
2.All users take longer trips over weekend and summer.


```{r}
All_trips_v3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(distance_of_ride = mean(ride_distance), .groups = 'drop') %>%
  arrange(day_of_week)
```

```{r}
All_trips_v3 %>%
  group_by(member_casual, month) %>%
  summarise(distance_of_ride = mean(ride_distance), .groups='drop') %>%
  arrange(month)
```
*Finding*
1. Membership ride's has slightly longer distance trip on Weekdays and casual ride's has slightly longer distance trip over weekend.
2. All users slightly longer distance trip in Monsoon season.

Finally, Let's find out how many riders use the same bike station for start point and end point(ride_distance=0)

```{r}
All_trips_v3 %>%
  group_by(member_casual) %>%
  summarise(number_of_rides = n(), .groups = 'drop')
```
```{r}
All_trips_v3 %>%
  group_by(member_casual) %>%
  filter(ride_distance < 1) %>%
  summarise(number_of_rides = n(), .groups = 'drop')
```
*Finding*

While 6% of casual riders return their bike to their start point, 3% of membership rider returns at their start point station.

```{r}

```


**Share**

I would like to share visualization which would allow executives to understand my conclusion easily.

```{r}
All_trips_v3 %>%
  group_by(member_casual,day_of_week) %>%
  summarise(number_of_rides =n(), .groups = 'drop') %>%
  ggplot(aes(x= day_of_week, y= number_of_rides ,fill = member_casual)) +
  geom_bar(position = "dodge",stat ="identity")
```

```{r}
All_trips_v3 %>%
  group_by(member_casual,month) %>%
  summarise(number_of_rides =n(), .groups = 'drop') %>%
  ggplot(aes(x= month, y= number_of_rides ,fill = member_casual)) +
  geom_bar(position = "dodge",stat ="identity")
```
```{r}
All_trips_v3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_ride_length = mean(ride_length), .groups = 'drop')%>%
  ggplot(aes(x=day_of_week, y=average_ride_length, fill =member_casual)) +
  geom_bar(position = "dodge", stat = "identity")
```
```{r}
All_trips_v3 %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_length = mean(ride_length), .groups = 'drop')%>%
  ggplot(aes(x=month, y=average_ride_length, fill =member_casual)) +
  geom_bar(position = "dodge", stat = "identity")
```

```{r}
All_trips_v3 %>%
  filter(ride_distance < 10000) %>%
  ggplot(aes(x=ride_distance, fill= member_casual)) +
  geom_histogram()
```

```{r}
All_trips_v3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_ride_distance = mean(ride_distance), .groups = 'drop')%>%
  ggplot(aes(x=day_of_week, y=average_ride_distance, fill =member_casual)) +
  geom_bar(position = "dodge", stat = "identity")
```

```{r}
All_trips_v3 %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_distance = mean(ride_distance), .groups = 'drop')%>%
  ggplot(aes(x=month, y=average_ride_distance, fill =member_casual)) +
  geom_bar(position = "dodge", stat = "identity")
```
*Analysis:*

1. It seems that the membership users travel the same average distance than the casual users, but    they have relatively longer rides.
2. Casual riders are more likely to return their bikes at the same station.
3. Membership rider's are more active on weekday, casual rider use the service more often over        weekend. It lead me to conclude that membership riders use this service for their commute while    casual rider use it for fun.


**Conclusion**

* The Casual users have leisure, and tourism rides mostly on weekends.
* The Annual users have commute or pragmatic rides during weekdays.

**Recommendation**

To convert casual riders into annual members, the following marketing strategies can be implemented:

1. We can provide offer and discount to casual member if they convert member for weekends and        monsoons.
2. We can increase rental prices for weekends for casual rides so that they have certain incentive   to convert to members.

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

