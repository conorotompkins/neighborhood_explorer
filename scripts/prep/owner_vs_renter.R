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

census_data_raw <- c(2010:2019) %>% 
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
  mutate(variable = "% of population in owner-occupied housing",
         NAME = str_c("Tract", GEOID, sep = " "),
         graph_type = "discrete",
         tract_year = 2010) %>% 
  select(GEOID, tract_year, NAME, year, variable, category, estimate, summary_est, moe)

census_data <- census_data_raw %>% 
  filter(category == "Population in owner-occupied housing") %>% 
  mutate(estimate_pct = estimate / summary_est,
         moe_pct = moe / estimate,
         moe_pct = case_when(is.infinite(moe_pct) ~ 0,
                             is.finite(moe_pct) ~ moe_pct)) %>% 
  select(-c(estimate, moe, category)) %>% 
  rename(estimate = estimate_pct,
         moe = moe_pct) %>% 
  mutate(units = "percent")

glimpse(census_data)

census_data %>% 
  write_csv("inputs/data_sources/owner_vs_renter.csv")

census_data %>% 
  filter(GEOID %in% c("42003472400", "42003472300")) %>% 
  ggplot(aes(year, estimate, color = GEOID, fill = GEOID, group = GEOID)) +
  geom_ribbon(aes(ymin = estimate - moe, ymax = estimate + moe), alpha = .3) +
  geom_line()

census_data %>% 
  filter(GEOID %in% c("42003051100")) %>% 
  ggplot(aes(year, estimate, color = GEOID, fill = GEOID, group = GEOID)) +
  geom_ribbon(aes(ymin = estimate - moe, ymax = estimate + moe), alpha = .3) +
  geom_line()
