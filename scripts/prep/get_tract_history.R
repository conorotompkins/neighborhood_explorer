library(tidyverse)
library(tidycensus)
library(sf)

options(tigris_use_cache = TRUE)

ac_geo <- c(2010, 2020) %>% 
  map({~get_acs(geography = "tract", 
               state = "Pennsylvania",
               county = "Allegheny County",
               variables = c(medincome = "B19013_001"),
               year = .x,
               geometry = T) %>% 
      mutate(census_year = .x) %>% 
      select(census_year, GEOID, geometry)},
      .id = "census_year")

dir.create("inputs/allegheny_county_tract_history")

ac_geo %>% 
  bind_rows() %>% 
  st_transform(4326) %>% 
  st_write("inputs/allegheny_county_tract_history/allegheny_county_tract_history.shp")