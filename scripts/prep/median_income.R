library(tidyverse)
library(tidycensus)
library(sf)

options(tigris_use_cache = TRUE)

c(2010:2019) %>% 
  set_names() %>% 
  map_dfr({~get_acs(geography = "tract", 
                state = "Pennsylvania",
                county = "Allegheny County",
                variables = c("Median Income" = "B19013_001"),
                year = .x,
                geometry = F) %>%
      mutate(NAME = str_c("Tract", GEOID, sep = " "),
             graph_type = "point_in_time",
             census_year = 2020)},
      .id = "year") %>% 
  select(GEOID, NAME, year, variable, estimate, moe, census_year) %>% 
  write_csv("inputs/data_sources/median_income.csv")