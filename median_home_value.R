#script to extract what % of a tracts population lives in owner-occupied vs renter-occupied housing

library(tidyverse)
library(tidycensus)
library(sf)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

acs_vars <- load_variables(year = 2021, dataset = "acs5")

dec_vars <- load_variables(year = 2020, dataset = "sf1")

glimpse(acs_vars)

acs_vars |>
  filter(name == "B25077_001")

acs_vars %>%
  #filter(geography == "tract") %>%
  filter(str_detect(concept, "MEDIAN VALUE")) %>%
  #distinct(concept, geography) %>%
  view()

acs_vars %>%
  filter(name == "B25077_001") %>%
  view()

#get census data for years 2010 through 2019
test <- get_decennial(geography = "tract",
        variables = "B25077_001",
        year = 2010,
        state = "PA",
        county = "Allegheny County",
        geometry = TRUE)

test

test |> 
  ggplot(aes(fill = estimate)) +
  geom_sf() +
  scale_fill_viridis_c()

census_data_raw <- c(2010:2020) %>% 
  set_names() %>% 
  map_dfr({~get_acs(geography = "tract",
                    variables = "B25077_001",
                    year = .x,
                    state = "PA",
                    county = "Allegheny County",
                    geometry = TRUE)},
      .id = "year") |> 
  rename(category = variable) |> 
  mutate(variable = "Owner-reported median home value",
         NAME = str_c("Tract", GEOID, sep = " "),
         graph_type = "discrete",
         tract_year = 2010) |> #tract year defines which decade the tract was created for
  select(GEOID, tract_year, NAME, year, variable, category, estimate, moe) |> 
  mutate(unit = "dollars")

glimpse(census_data_raw)

census_data_raw |> 
  ggplot(aes(fill = estimate)) +
  geom_sf(color = NA) +
  facet_wrap(vars(year)) +
  scale_fill_viridis_c() +
  theme_void()
