# load require packages
library(tidyverse)
library(sf)
library(nngeo)
library(units)
library(pacman)
library(lubridate)

# load data
station<- st_read("data/phl_bike_stations.shp")
add <- read_csv("data/indego-stations-2024-07-01.csv")
rider <- read_csv("data/indego-trips-2024-q2.csv")
neighborhood <- st_read("data/philadelphia-neighborhoods.shp")

# add additional info into station
station <- station %>%
  left_join(., add, by = c("id" = "Station_ID"))

## cleaning the rider table
rider <- rider %>%
  select(duration, 
         start_time,
         end_time,
         start_station,
         end_station, 
         trip_route_category, 
         bike_type) 

## The 10 stations with the HIGHEST number of STARTS (stations that are origins)

rider_starter <- rider %>%
  group_by(start_station) %>%
  summarize(n = n()) %>%
  arrange(desc(n))


head(rider_starter, 10)

## The 10 stations with the LOWEST number of STARTS (stations that are origins)

tail(rider_starter, 10)

## A map that displays the number of STARTS by station that shows the range of values
start_station_join <- station %>%
  left_join(., rider_starter, by = c("id" = "start_station"))


## The 10 stations with the HIGHEST number of ENDS (stations that are destinations)

rider_ender <- rider %>%
  group_by(end_station) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
head(rider_ender, 10)


## The 10 stations with the LOWEST number of ENDS (stations that are destinations)
tail(rider_ender, 10)


## A map that displays the number of ENDS by station that shows the range of values

end_station_join <- station %>%
  left_join(., rider_ender, by = c("id" = "end_station"))

## The 5 STARTING STATIONS with the longest AVERAGE trip durations
average_duration_by_station <- rider %>%
  group_by(start_station) %>%
  summarize(average_duration = mean(duration, na.rm = TRUE))%>%
  arrange(desc(average_duration))

head(average_duration_by_station, 5)



## the 5 STARTING stations with the lowest AVERAGE trip durations
tail(average_duration_by_station, 5)


## What stations,BASED ON THE YEAR THEY WERE ACTIVATED, had the most trips?

staion_ender_starter <- left_join(rider_ender, rider_starter, by = c("end_station" = "start_station")) %>%
  mutate(total = n.x + n.y) %>%
  select("end_station", "total")%>%
  rename("id" = "end_station")

station_complete <- station %>%
  left_join(., staion_ender_starter, by = c("id" = "id"))



station_complete <- station_complete %>%
  rename("Activation_date"="Day of Go_live_date")



station_complete <- station_complete %>%
  mutate(Acti_date = mdy(Activation_date)) %>%
  mutate(year = year(Acti_date))



station_trip_by_year <-station_complete %>%
  group_by(year,id) %>%
  summarize(total_trip = sum(total), .groups = "drop")



top_stations_by_year <- station_trip_by_year %>%
  group_by(year) %>%
  filter(total_trip== max(total_trip))




## What are the 5 neighborhoods with the most TOTAL TRIPS and what are the 5 neighborhoods with the most TRIPS PER STATION?
neighborhood <-neighborhood %>%
  st_transform(crs=st_crs(station_complete))
#Check the invalid geometries in the neighborhood data
invalid_neighborhoods <- neighborhood %>%
  filter(!st_is_valid(.))

# Print out how many invalid geometries there are
print(paste("Number of invalid geometries:", nrow(invalid_neighborhoods)))

neighborhood <- st_make_valid(neighborhood)

#Spatial join the station data with the neighborhood data
station_complete <- station_complete %>%
  filter(!is.na(total))

station_in_neighborhood <- station_complete %>%
  st_join(neighborhood, join= st_within) 

station_in_neighborhood <- station_in_neighborhood %>%
  group_by(NAME) %>%
  summarize(total_trip = sum(total), total_station = n()) %>%
  mutate(trip_per_station = total_trip/total_station) %>%
  arrange(desc(total_trip))


## A categorical map that colors the stations by three categories for trip duration. You determine what values correspond to which class, but the map should have three categories: short, medium and long.
average_duration_by_station <- average_duration_by_station %>%
  mutate(average_duration_cat = case_when(
    average_duration <= 20 ~ "short",
    average_duration > 20 & average_duration <= 30 ~ "medium",
    average_duration > 30 ~ "long"
  ))%>%
  rename("id" = "start_station")

average_duration_staion_join <- left_join(station, average_duration_by_station, by = c("id" = "id"))
