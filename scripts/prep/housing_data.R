#script to process housing unit data from https://osf.io/fzv5e/?view_only=

library(tidyverse)
library(sf)
library(units)

#HHUUD10.gdb.zip https://osf.io/fzv5e/files/osfstorage/613800da28b37600377cf675

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

#fix year column
year_lookup <- ac_housing_hu %>% 
  distinct(year) %>% 
  mutate(year_fixed = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2019))

ac_housing_hu <- ac_housing_hu %>% 
  left_join(year_lookup) %>% 
  select(-year) %>% 
  rename(year = year_fixed,
         estimate = housing_units) %>% 
  mutate(variable = "Estimated Housing Units",
         estimate = round(estimate, 0)) %>% 
  select(GEOID, variable, year, estimate) %>% 
  mutate(tract_year = 2010,
         unit = "count")

ac_housing_hu %>% 
  write_csv("inputs/data_sources/housing_data.csv")
