library(tidyverse)
library(tidycensus)
library(sf)

acs_2019 <- load_variables(year = 2019, dataset = "acs5")
acsVariables <- c(totpop = "B01001_001",
                  asian = "B03002_006", #Asian Non Hispanic alone,
                  black = "B03002_004", #Black non-Hispanic alone
                  hisp_latx = "B03003_003", #Hispanic/Latine
                  white = "B03002_003", #White Non-Hispanic
                  amerind_natalask = "B03002_005", #American Indian/Alaskan Native
                  other = "B03002_008", #other race
                  tworace = "B03002_009", #two or more reaces
                  occ = "B25003_001", #occupied housing units
                  own = "B25003_002", #occupied owned housing units
                  rent = "B25003_003", #occupied rented housing units
                  totCommute ="B08006_001", #total number of commuters
                  pubTrans ="B08006_008", #public transportation riders
                  bikers ="B08006_014", #cyclists
                  medInc = "B19013_001") #median income
mc_2019 <- get_acs(geography="tract", variables = acsVariables,
                   state = "NC", county = "Mecklenburg", survey =
                     "acs5", year = 2019, output = "wide", geometry =
                     TRUE)
mc_2019 <- mc_2019 %>%
  mutate(transit_prop = pubTransE/totpopE, renter_prop = rentE/occE)
st_write(obj = mc_2019, dsn = "mc_2019.gpkg", delete_dsn = TRUE)
