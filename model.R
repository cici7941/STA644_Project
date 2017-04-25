##Data Pre-process
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)

trip = fread('babs_open_data_year_2/201508_trip_data.csv')
station = fread('babs_open_data_year_2/201508_station_data.csv')
status <-fread("babs_open_data_year_2/201508_status_data.csv")
head(trip)
df = trip %>% 
  mutate(date = floor_date(mdy_hm(`Start Date`),'day')) %>%
  mutate(hour_of_day = hour(mdy_hm(`Start Date`))) %>%
  group_by(`Start Terminal`, date, hour_of_day) %>%
  summarise(`Number of Trips` = n())

agg = merge(df, station, by.x = 'Start Terminal', by.y = 'station_id') %>%
  mutate(day_of_week = wday(date))
