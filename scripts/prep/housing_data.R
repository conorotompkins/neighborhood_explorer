library(tidyverse)
library(sf)
library(units)

ac_housing <- st_read("inputs/raw/HHUUD10.gdb.zip") %>% 
  rename(geometry = Shape) %>% 
  mutate(UY1 = na_if(UY1, 2035),
         UY2 = na_if(UY2, 2035)) %>% 
  filter(STATE == "PA",
         COUNTY == "Allegheny") %>% 
  select(STATE, COUNTY, GEOID10, UY1, UY2, starts_with("hu"), starts_with("sqmi"), starts_with("pdev")) %>% 
  rename(GEOID = GEOID10) %>% 
  st_drop_geometry() %>% 
  as_tibble()

ac_housing_hu <- ac_housing %>% 
  select(GEOID, starts_with("hu")) %>% 
  pivot_longer(cols = starts_with("hu"), names_to = "year", values_to = "housing_units")

year_lookup <- ac_housing_hu %>% 
  distinct(year) %>% 
  mutate(year_fixed = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2019))

ac_housing_hu <- ac_housing_hu %>% 
  left_join(year_lookup) %>% 
  select(-year) %>% 
  rename(year = year_fixed,
         estimate = housing_units) %>% 
  mutate(variable = "housing_units") %>% 
  select(GEOID, variable, year, estimate) %>% 
  mutate(graph_type = "time_series")

ac_housing_hu %>% 
  write_csv("inputs/data_sources/housing_data.csv")
