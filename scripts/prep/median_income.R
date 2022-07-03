library(tidyverse)
library(tidycensus)
library(sf)

options(tigris_use_cache = TRUE)

get_acs(geography = "tract", 
                  state = "Pennsylvania",
                  county = "Allegheny County",
                  variables = c(medincome = "B19013_001"),
                  year = 2020,
                  geometry = T) %>% 
  select(GEOID, geometry, variable, estimate, moe) %>% 
  mutate(NAME = str_c("Tract", GEOID, sep = " ")) %>% 
  st_drop_geometry() %>% 
  write_csv("inputs/data_sources/median_income.csv")
