### Assignment 2
library(pacman)
p_load(sf, tmap, tigris, tidyverse, RColorBrewer)

violations <- read_csv("https://phl.carto.com/api/v2/sql?&filename=violations&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*,%20ST_Y(the_geom)%20AS%20lat,%20ST_X(the_geom)%20AS%20lng%20FROM%20violations%20WHERE%20violationdate%20%3E=%20%272019-01-01%27")

violation_dupes<- violations %>%
  group_by(casenumber) %>%
  tally() %>%
  arrange(desc(n)) %>%
  head()

viol_check <- violations %>%
  filter(casenumber == "CF-2023-010908") %>%
  select(casenumber, casetype:violationnumber, violationcode) %>%
  head()

# get the rows with duplicate violation numbers
viol_check2 <- violations %>%
  group_by(violationnumber) %>%
  tally() %>%
  filter( n > 1)

# Get the rows with duplicate violation numbers
dupe_rows <- as_vector(viol_check2$violationnumber)

# get all the duplicate rows
viol_check3 <- violations %>%
  filter(violationnumber %in% c(dupe_rows))

check <- viol_check3 %>%
  filter(address== "3220 N 16TH ST")

# get the actual number (true data)
violations <-violations %>%
  distinct(violationnumber, .keep_all = TRUE)

violations_ct <- violations %>%
  count(violationcodetitle)%>%
  arrange(desc(n)) %>%
  slice_head(n=10)
