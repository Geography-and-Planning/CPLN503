library(tidyverse)
library(duckdb)
library(tidycensus)
library(sf)

setwd("C:/Users/zyang/OneDrive/desktop/nov7th")

#load the data

con <- dbConnect(duckdb::duckdb(), dbdir = "raw_data.duckdb")

dbListTables(con)

person<- dbGetQuery(con, "SELECT * FROM person2021")

person<- person%>%
  filter(INJ_SEV==4)

person<- person%>%
  mutate(county_code= sprintf("%02d%03d", person$STATE, person$COUNTY) )

#prepare
personfilter<-person%>%
  group_by(STATE, STATENAME,county_code) %>%
  tally()

continetal<-personfilter%>%
  filter(STATENAME %in% c("Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"))

county_population <- get_acs(
  geography = "county",
  variables = "B01003_001", # Total population variable
  year = 2022,
  survey = "acs5",
  geometry = TRUE
)%>%
  select(GEOID, estimate, geometry)

county_fatalities<-county_population%>%
  left_join(continetal, by=c("GEOID"="county_code"))%>%
  mutate(fatality_rate= n/estimate*1000)

st_write(county_fatalities, "county_fatalities.gpkg")

dbDisconnect(con)

svi<-read_csv("svi_interactive_map.csv")
svi_FL<- svi%>%
  filter(STATE=="Florida")

write.csv(svi_FL, "svi_FL.csv")




personfilter1<-person%>%
  filter(PER_TYP==5)%>%
  group_by(STATE, STATENAME,county_code) %>%
  tally()

fl_motorcyclist<-personfilter1%>%
  filter(STATENAME=="Florida")

couty_pedestrian<-get_acs(
  geography = "county",
  state = "FL",
  variables = "B08301_010E", # Total population variable
  year = 2022,
  survey = "acs5",
  geometry = TRUE
)%>%
  select(GEOID, estimate, geometry)

fl_ped<-left_join(couty_pedestrian, fl_motorcyclist, by=c("GEOID"="county_code"))%>%
  mutate(fatality_rate= n/estimate*1000)

st_write(fl_ped, "fl_ped.gpkg")
