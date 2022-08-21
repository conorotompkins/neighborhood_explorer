library(tidyverse)
library(tidycensus)
library(sf)

options(tigris_use_cache = TRUE)

acs1_vars <- load_variables(2019, 'acs1') %>% 
  mutate(across(c(label, concept), str_to_lower))

acs1_vars %>% 
  filter(str_detect(name, "B08301"))

all_transit_vars <- c("Drove alone (car/truck/van)" = "B08301_003", 
                      "Carpooled (car/truck/van)" = "B08301_004", 
                      "Public transportation (excluding taxicab)" = "B08301_010", 
                      "Taxicab" = "B08301_016", 
                      "Motorcyle" = "B08301_017", 
                      "Bicycle" = "B08301_018", 
                      "Walked" = "B08301_019", 
                      "Other means" = "B08301_020",
                      "Worked from home" = "B08301_021")

acs1_vars %>% 
  filter(name %in% all_transit_vars)

c(2010:2019) %>% 
  set_names() %>% 
  map_dfr({~get_acs(geography = "tract", 
                                         variables = all_transit_vars,
                                         summary_var = "B08301_001",
                                         year = .x,
                                         state = "PA",
                                         county = "Allegheny",
                                         geometry = F)},
      .id = "year") %>% 
  rename(category = variable) %>% 
  mutate(variable = "Commuters",
         NAME = str_c("Tract", GEOID, sep = " "),
         graph_type = "discrete",
         tract_year = 2010) %>% 
  select(GEOID, NAME, year, variable, category, estimate, moe, tract_year) %>% 
  write_csv("inputs/data_sources/commute_modes.csv")
