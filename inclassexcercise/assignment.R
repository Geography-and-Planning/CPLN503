library(nngeo)
library(tidyverse)
library(sf)
library(units)
library(pacman)

setwd("D:/UPenn/cpln5030/2024-09-26_In_Class_Excercise")
philly_gdb_layers <- st_layers("2024-09-26_Data.gdb")

philly_gdb_layers

parcels <- st_read("2024-09-26_Data.gdb", layer = "Parcels",quiet=TRUE)
parks <- st_read("2024-09-26_Data.gdb", layer = "Parks",quiet=TRUE)
crimes<- st_read("2024-09-26_Data.gdb", layer = "Crimes",quiet=TRUE)

johns <- parcels %>% 
  filter(str_detect(string = OWNER1, pattern = "^JOHN\\s"))

johns <- johns %>% 
  mutate(parcel_acres = st_area(.)) %>% 
  mutate(parcel_acres = set_units(x = parcel_acres, value = "acres"))

johns <-johns %>%
  mutate(price_per_acre = as.numeric(SALE_PR/parcel_acres))%>%
  filter(price_per_acre >= 500)

parks500 <- st_buffer(x= parks, dist=500)
parks1000 <- st_buffer(x= parks, dist=1000)

parks500 <- st_union(parks500)
parks1000 <- st_union(parks1000)

parks_diff<- st_difference(parks1000, parks500)

johns <- johns[parks_diff,]

johns <- johns %>% 
  filter(str_detect(string = DESC, pattern = "^ROW"))

crimes <- crimes %>% 
  filter(!is.na(lng) | !is.na(lat)) %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
  st_transform(st_crs(johns))

crim <- crimes %>% 
  filter(str_detect(string = text_general_code, pattern = regex("Assault")))

crimes1000 <- st_buffer(x= crim, dist=1000)
crimes1000 <- st_union(crimes1000)

crime_parcels <- st_intersects(johns, crimes1000)
crime_logical <- lengths(crime_parcels) == 0
final_parcels <- johns[crime_logical, ]

sum(final_parcels$parcel_acres)