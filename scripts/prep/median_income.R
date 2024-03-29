#script to extract median household income by tract

library(tidyverse)
library(tidycensus)
library(sf)

options(tigris_use_cache = TRUE)

acs_vars <- load_variables(year = 2020, dataset = "acs5")

glimpse(acs_vars)

acs_vars |> 
  filter(name == "B19013_001")

c(2010:2019) %>% 
  set_names() %>% 
  map_dfr({~get_acs(geography = "tract", 
                state = "Pennsylvania",
                county = "Allegheny County",
                variables = c("Median Household Income" = "B19013_001"),
                year = .x,
                geometry = F) %>%
      mutate(NAME = str_c("Tract", GEOID, sep = " "),
             graph_type = "point_in_time",
             tract_year = 2010)},
      .id = "year") %>% 
  select(GEOID, NAME, year, variable, estimate, moe, tract_year) %>% 
  mutate(unit = "dollars") %>% 
  write_csv("inputs/data_sources/median_household_income.csv")