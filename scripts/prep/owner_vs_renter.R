library(tidyverse)
library(tidycensus)
library(sf)

options(tigris_use_cache = TRUE)

acs_vars <- load_variables(year = 2020, dataset = "acs5")

glimpse(acs_vars)

# acs_vars %>% 
#   filter(geography == "tract") %>% 
#   filter(str_detect(concept, "UNITS")) %>% 
#   distinct(concept, geography) %>% 
#   view()

# acs_vars %>% 
#   filter(concept == "TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE BY YEAR HOUSEHOLDER MOVED INTO UNIT") %>% 
#   view()

census_data <- c(2010:2019) %>% 
  set_names() %>% 
  map_dfr({~get_acs(geography = "tract", 
                    state = "Pennsylvania",
                    county = "Allegheny County",
                    variables = c("Population in owner-occupied housing" = "B25026_002",
                                  "Population in renter-occupied housing" = "B25026_009"),
                    summary_var = "B25026_001",
                    year = .x,
                    geometry = F) %>%
      mutate(NAME = str_c("Tract", GEOID, sep = " "),
             graph_type = "point_in_time",
             tract_year = 2010)},
      .id = "year") %>% 
  rename(category = variable) %>% 
  mutate(variable = "Population",
         NAME = str_c("Tract", GEOID, sep = " "),
         graph_type = "discrete",
         tract_year = 2010) %>% 
  select(GEOID, tract_year, NAME, year, category, estimate, summary_est, moe, summary_moe) %>% 
  mutate(pct = estimate / summary_est)

glimpse(census_data)

census_data %>% 
  write_csv("inputs/data_sources/owner_vs_renter.csv")

census_data %>% 
  filter(GEOID %in% c("42003472400", "42003472300")) %>% 
  ggplot(aes(year, estimate, color = category, fill = category, group = category)) +
  geom_ribbon(aes(ymin = estimate - moe, ymax = estimate + moe), alpha = .3) +
  geom_line() +
  facet_wrap(~GEOID)
      