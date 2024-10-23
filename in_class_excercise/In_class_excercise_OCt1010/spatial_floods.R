library(tidyverse)
library(sf)
library(tidycensus)


pluto <- st_read(("/Users/apple/Desktop/CPLN503/in_class_excercise/In_class_excercise_octo17th/MapPLUTO24v3_1.gdb"), 
                 query = "select * from MapPLUTO_24v3_1_clipped where Borough = 'BK'", quiet = TRUE)

pluto <- pluto %>%
  mutate(ARA = case_when(ResArea == 0 & UnitsRes != 0 ~ 1 * (BldgArea * (UnitsRes/UnitsTotal)) + ResArea,
                         ResArea > 0 ~ ResArea,                         
                         TRUE ~ 0))

bk_bg <- get_acs(geography = "block group", 
                 variables = c(bg_tot_pop = "B03002_001", 
                               bg_white_nh = "B03002_003" , 
                               bg_black_nh = "B03002_004", 
                               bg_asian_nh = "B03002_006" , 
                               bg_native_hawaiian = "B03002_007",
                               bg_other_nh = "B03002_008", 
                               bg_two_more_nh = "B03002_009",
                               bg_hispanic = "B03002_012"), 
                 year = 2021, output = "wide", 
                 geometry = TRUE, state = "NY", 
                 county = "Kings", survey = "acs5", 
                 progress = FALSE)

bk_tracts <-get_acs(geography = "tract", 
                    variables = c(tract_tot_pop = "B03002_001",
                                  tract_white_nh = "B03002_003", 
                                  tract_black_nh = "B03002_004",   
                                  tract_asian_nh = "B03002_006", 
                                  tract_native_hawaiian = "B03002_007",
                                  tract_other_nh = "B03002_008" , 
                                  tract_two_more_nh = "B03002_009",
                                  tract_hispanic = "B03002_012" ), 
                    year = 2021, output = "wide", 
                    geometry = TRUE, state = "NY", 
                    county = "Kings", survey = "acs5", 
                    progress = FALSE)

pluto <- st_transform(pluto, crs = st_crs(bk_bg))

pluto <- pluto %>%
  st_join(bk_bg %>% select(block_group_2021 = GEOID, 3, 5, 7, 9, 11, 13, 15, 17))

pluto <- pluto %>%
  st_join(bk_tracts %>%
            select(tract_2021 = GEOID, 3, 5, 7, 9, 11, 13, 15, 17))

pluto <- pluto %>%
  select(Borough, Address, OwnerName, LotArea, BldgArea,
         ResArea, UnitsRes, ARA, block_group_2021, 
         starts_with("bg_"), tract_2021, starts_with("tract_"))

bk_bg_units <- pluto |> 
  as_tibble() |> 
  group_by(block_group_2021) |> 
  summarise(bg_res_units = sum(UnitsRes, na.rm = TRUE), 
            bg_ARA = sum(ARA, na.rm = TRUE))

bk_tract_units <- pluto |> 
  as_tibble() |> 
  group_by(tract_2021) |> 
  summarise(tract_res_units = sum(UnitsRes, na.rm = TRUE), 
            tract_ARA = sum(ARA, na.rm = TRUE))

pluto <- pluto |> 
  left_join(bk_bg_units)

pluto <- pluto |> 
  left_join(bk_tract_units)

pluto <- pluto %>% 
  ungroup() %>% 
  mutate(bg_tot_dasym = case_when(UnitsRes == 0 & ResArea != 0 ~ 
                                    bg_tot_popE * (ARA/bg_ARA),
                                  UnitsRes > 0 ~ bg_tot_popE * (UnitsRes/bg_res_units),
                                  TRUE ~ 0),
         tract_tot_dasym = case_when(UnitsRes == 0 & ResArea != 0 ~ 
                                       tract_tot_popE * (ARA/tract_ARA),
                                     UnitsRes > 0 ~ tract_tot_popE * (UnitsRes/tract_res_units),
                                     TRUE ~ 0))

tract_ru_totals <- pluto |> 
  as_tibble() |> 
  group_by(tract_2021) |> 
  summarise(tract_ru_est = sum(tract_tot_dasym, na.rm = TRUE))

tract_ru_totals <- tract_ru_totals |> 
  inner_join(bk_tracts |> 
               select(GEOID, tract_tot_popE), by = c("tract_2021" = "GEOID"))


floods <- st_read("https://data.cityofnewyork.us/api/geospatial/ezfn-5dsb?method=export&format=GeoJSON", )


floods <- st_make_valid(floods)

floods <- floods |> 
  st_transform(crs = st_crs(pluto)) |> 
  st_union(by_feature = FALSE) |> 
  st_as_sf() 

floods <- floods |> 
  mutate(flood_plain = "100-Year") |> 
  rename(geometry = x)

bk_bg_floods <- bk_bg |> 
  st_join(floods) |> 
  as_tibble() |> 
  group_by(flood_plain) |> 
  summarise(flood_pop_bg = sum(bg_tot_popE, na.rm = TRUE)) |> 
  mutate(flood_plain = if_else(is.na(flood_plain), "Not in plain", flood_plain))

bk_tract_flood <- bk_tracts |> 
  st_join(floods) |> 
  as_tibble() |> 
  group_by(flood_plain) |> 
  summarise(flood_pop_tract = sum(tract_tot_popE, na.rm = TRUE)) |> 
  mutate(flood_plain = if_else(is.na(flood_plain), "Not in plain", flood_plain))

pluto_res <- pluto |> 
  filter(bg_tot_dasym > 0 | tract_tot_dasym > 0)

pluto_non_res <- pluto |> 
  filter((is.na(ResArea) | ResArea == 0) & (UnitsRes == 0 | is.na(UnitsRes)))

pluto_floods <- pluto_res |> 
  st_join(floods)

pluto_floods <- pluto_floods |> 
  as_tibble() |> 
  group_by(flood_plain) |> 
  summarise(flood_pop_bg_dasym = sum(bg_tot_dasym, na.rm = TRUE),
            flood_pop_tract_dasym = sum(tract_tot_dasym, na.rm = TRUE)) |> 
  mutate(flood_plain = if_else(is.na(flood_plain), "Not in plain", flood_plain))


pluto_floods <- pluto_floods |> 
  left_join(bk_tract_flood) |> 
  left_join(bk_bg_floods)
