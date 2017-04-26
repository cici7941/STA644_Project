##Data Pre-process
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)

trip = fread('babs_open_data_year_2/201508_trip_data.csv')
station = fread('babs_open_data_year_2/201508_station_data.csv')
#status <-fread("babs_open_data_year_2/201508_status_data.csv")
head(trip)
df = trip %>% 
  mutate(date = floor_date(mdy_hm(`Start Date`),'day')) %>%
  mutate(hour_of_day = hour(mdy_hm(`Start Date`))) %>%
  group_by(`Start Terminal`, date, hour_of_day) %>%
  summarise(`Number of Trips` = n())

agg = merge(df, station, by.x = 'Start Terminal', by.y = 'station_id') %>%
  mutate(day_of_week = wday(date))


library(fields)
library(maps)
states <- map_data("state")
bayArea <- subset(states, region %in% c("california"))

counties <- map_data("county")
ca_county <- subset(counties, region == "california")
bay_name <- c("san francisco", "san mateo","santa clara")
bay_county <- subset(ca_county,subregion %in% bay_name)

r = raster(nrows=50, ncol=100,
           xmn = min(bay_county$long), xmx = max(bay_county$long),
           ymn = min(bay_county$lat), ymx = max(bay_county$lat))
bay = rasterize(bay_county[,c('long','lat')],r)
cells = which(!is.na(bay[]))
pred_coords = xyFromCell(r, cells)
coords = dplyr::select(agg, long, lat) %>% as.matrix()
tps = Tps(x = coords, Y=agg$`Number of Trips`)
pm25_pred = r
pm25_pred[cells] = predict(tps, pred_coords)

plot(pm25_pred)
points(coords, pch=16, cex=0.5)
#library(ggplot2)
#ggplot(pm25_pred, aes(long,lat))+geom_raster(aes(fill = values))
