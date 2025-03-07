library(sf)
library(tidyverse)
library(tidycensus)

pluto <- st_read(("C:/Users/zyang/OneDrive/desktop/CPLN503/in_class_excercise/In_class_excercise_OCt1010/MapPLUTO24v3_1.gdb"),
                 query = "select * from MapPLUTO_24v3_1_clipped where Borough = 'BK'", quiet = TRUE)

pluto <- pluto %>%
  mutate(ARA = case_when(ResArea == 0 & UnitsRes != 0 ~ 1 * (BldgArea * (UnitsRes/UnitsTotal)) + ResArea,
                         ResArea > 0 ~ ResArea,
                         TRUE ~ 0))

bk_bg <- get_acs(geography = "block group",
                 variables = c(bg_tot_pop = "B03002_001",
                               bg_white_nh = "B03002_003" ,
                               bg_black_nh = "B03002_004"),
                 year = 2021, output = "wide",
                 geometry = TRUE, state = "NY",
                 county = "Kings", survey = "acs5",
                 progress = FALSE)

bk_tracts <-get_acs(geography = "tract",
                    variables = c(tract_tot_pop = "B03002_001",
                                  tract_white_nh = "B03002_003",
                                  tract_black_nh = "B03002_004"),
                    year = 2021, output = "wide",
                    geometry = TRUE, state = "NY",
                    county = "Kings", survey = "acs5",
                    progress = FALSE)


pluto <- st_transform(pluto, crs = st_crs(bk_bg))

pluto <- pluto %>%
  st_join(bk_bg %>% select(block_group_2021 = GEOID, 3, 5, 7))

pluto <- pluto %>%
  st_join(bk_tracts %>%
            select(tract_2021 = GEOID, 3, 5, 7))
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
  mutate(bg_bl_dasym = case_when(UnitsRes == 0 & ResArea != 0 ~
                                    bg_black_nhE * (ARA/bg_ARA),
                                  UnitsRes > 0 ~ bg_black_nhE * (UnitsRes/bg_res_units),
                                  TRUE ~ 0),
         tract_bl_dasym = case_when(UnitsRes == 0 & ResArea != 0 ~
                                       tract_black_nhE * (ARA/tract_ARA),
                                     UnitsRes > 0 ~ tract_black_nhE * (UnitsRes/tract_res_units),
                                     TRUE ~ 0))


tract_ru_bl <- pluto |>
  as_tibble() |>
  group_by(tract_2021) |>
  summarise(tract_bl_est = sum(tract_bl_dasym, na.rm = TRUE))

tract_ru_bl <- tract_ru_bl |>
  inner_join(bk_tracts |>
               select(GEOID, tract_black_nhE), by = c("tract_2021" = "GEOID"))

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
  summarise(flood_bl_bg = sum(bg_black_nhE, na.rm = TRUE)) |>
  mutate(flood_plain = if_else(is.na(flood_plain), "Not in plain", flood_plain))

bk_tract_flood <- bk_tracts |>
  st_join(floods) |>
  as_tibble() |>
  group_by(flood_plain) |>
  summarise(flood_bl_tract = sum(tract_black_nhE, na.rm = TRUE)) |>
  mutate(flood_plain = if_else(is.na(flood_plain), "Not in plain", flood_plain))

pluto_res <- pluto |>
  filter(bg_bl_dasym > 0 | tract_bl_dasym > 0)

pluto_non_res <- pluto |>
  filter((is.na(ResArea) | ResArea == 0) & (UnitsRes == 0 | is.na(UnitsRes)))

pluto_floods <- pluto_res |>
  st_join(floods)

pluto_floods <- pluto_floods |>
  as_tibble() |>
  group_by(flood_plain) |>
  summarise(flood_bl_bg_dasym = sum(bg_bl_dasym, na.rm = TRUE),
            flood_bl_tract_dasym = sum(tract_bl_dasym, na.rm = TRUE)) |>
  mutate(flood_plain = if_else(is.na(flood_plain), "Not in plain", flood_plain))


pluto_floods <- pluto_floods |>
  left_join(bk_tract_flood) |>
  left_join(bk_bg_floods)
