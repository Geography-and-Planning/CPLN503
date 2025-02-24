library(pacman)
p_load(sf, tidyverse)
library(sf)

setwd("C:/Users/zyang/OneDrive/文档/In_Class_Exercise_Sept19th")

schools <- st_read("schools.dbf", quiet = TRUE)

school_data <-st_read("Schools_Data.dbf", quiet = TRUE)


school_data <- school_data %>% 
  mutate(UnitID = as.character(UnitID))

schools <- schools %>% 
  mutate(UnitIDs = as.character(UnitIDs))

schools <- schools %>%
  inner_join(school_data, by =c("UnitIDs" = "UnitID"))

schools <- schools %>%
  mutate(low_inc_per = LowIncUG/TotalUG)

schools <-schools %>%
  mutate(state_dependent= if_else(StatePop>= 0.65,"High State Enrollment","Low State Enrollment"))

schl_lowinc_earn <- schools %>%
  group_by(RiskCatego) %>%
  summarize(low_inc_median= median(low_inc_per), med_earnings= median(MedEarn))

schools_sf <- schools %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

states <- st_read("states.shp", quiet = TRUE)
msa <- st_read("SelectMSA.shp", quiet = TRUE)

states <- st_transform(states, crs = st_crs(schools_sf))
msa <- st_transform(msa, crs = st_crs(schools_sf))

# spatial join

schools_st_sf <- schools_sf %>%
  st_join(states, join = st_within)

schools_st_msa_sf <- schools_st_sf %>%
  st_join(msa, join = st_within)


tx_mi_il <- schools_st_sf %>% 
  filter(STATE %in% c("Texas", "Michigan", "Illinois"))

tmi_summary <- tx_mi_il %>% 
  as_tibble() %>% 
  group_by(STATE, RiskCatego) %>% 
  summarise(low_inc = median(low_inc_per), 
            med_earnings = median(MedEarn))

#ggplot(schools_sf) +
#  geom_sf(aes(color = "RiskCatego")) + 
#  theme_minimal() 
#ggplot(schools_sf) +
#  geom_sf(aes(color = RiskCatego)) + 
#  theme_minimal() +
#  scale_color_manual(values = c("red", "blue", "green", "yellow", "purple")) +
#  labs(title = "Risk Category of Schools", 
#       color = "Risk Category") 


#import the data
state_Data_join <- st_read("State_Data_Join.dbf", quiet = TRUE)
MSA_Data_join <- st_read("MSA_Data_Join.dbf", quiet = TRUE)

#transform the data to sf file
head(state_Data_join)
state_join_sf <- state_Data_join %>% 
  st_as_sf(coords = c("Longitude_", "Latitude__"), crs = 4326)

MSA_join_sf <- MSA_Data_join %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

states <- st_transform(states, crs = st_crs(state_join_sf))
state_st_sf <- state_join_sf %>%
  st_join(states, join = st_within)
msa <- st_transform(msa, crs = st_crs(MSA_join_sf))
MSA_join_sf <- MSA_join_sf %>%
  st_join(msa, join = st_within)

st_write(obj=state_st_sf, dsn="state_st_sf.shp", delete_dsn=TRUE)
st_write(obj=MSA_join_sf, dsn="MSA_join_sf.shp", delete_dsn=TRUE)
st_write(obj=schools_st_msa_sf, dsn="schools_st_msa_sf.shp", delete_dsn=TRUE)
st_write(obj=schools_st_sf, dsn="schools_st_sf.shp", delete_dsn=TRUE)
