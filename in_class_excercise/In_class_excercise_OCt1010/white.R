pluto <- pluto %>% 
  ungroup() %>% 
  mutate(bg_his_dasym = case_when(UnitsRes == 0 & ResArea != 0 ~ 
                                   bg_hispanicE * (ARA/bg_ARA),
                                 UnitsRes > 0 ~ bg_hispanicE * (UnitsRes/bg_res_units),
                                 TRUE ~ 0),
         tract_his_dasym = case_when(UnitsRes == 0 & ResArea != 0 ~ 
                                      tract_hispanicE * (ARA/tract_ARA),
                                    UnitsRes > 0 ~ tract_hispanicE * (UnitsRes/tract_res_units),
                                    TRUE ~ 0))

tract_ru_his <- pluto |> 
  as_tibble() |> 
  group_by(tract_2021) |> 
  summarise(tract_his_est = sum(tract_his_dasym, na.rm = TRUE))

tract_ru_his <- tract_ru_his |> 
  inner_join(bk_tracts |> 
               select(GEOID, tract_hispanicE), by = c("tract_2021" = "GEOID"))

bk_bg_floods <- bk_bg |> 
  st_join(floods) |> 
  as_tibble() |> 
  group_by(flood_plain) |> 
  summarise(flood_his_bg = sum(bg_hispanicE, na.rm = TRUE)) |> 
  mutate(flood_plain = if_else(is.na(flood_plain), "Not in plain", flood_plain))

bk_tract_flood <- bk_tracts |> 
  st_join(floods) |> 
  as_tibble() |> 
  group_by(flood_plain) |> 
  summarise(flood_his_tract = sum(tract_hispanicE, na.rm = TRUE)) |> 
  mutate(flood_plain = if_else(is.na(flood_plain), "Not in plain", flood_plain))

pluto_res <- pluto |> 
  filter(bg_his_dasym > 0 | tract_his_dasym > 0)

pluto_non_res <- pluto |> 
  filter((is.na(ResArea) | ResArea == 0) & (UnitsRes == 0 | is.na(UnitsRes)))

pluto_floods <- pluto_res |> 
  st_join(floods)

pluto_floods <- pluto_floods |> 
  as_tibble() |> 
  group_by(flood_plain) |> 
  summarise(flood_his_bg_dasym = sum(bg_his_dasym, na.rm = TRUE),
            flood_his_tract_dasym = sum(tract_his_dasym, na.rm = TRUE)) |> 
  mutate(flood_plain = if_else(is.na(flood_plain), "Not in plain", flood_plain))


pluto_floods <- pluto_floods |> 
  left_join(bk_tract_flood) |> 
  left_join(bk_bg_floods)
