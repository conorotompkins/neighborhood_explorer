library(tidyverse)
library(tidycensus)
library(sf)

options(tigris_use_cache = TRUE)

acs1_vars <- load_variables(2019, 'acs1') %>% 
  mutate(across(c(label, concept), str_to_lower))

all_transit_vars <- c("B08301_003", 
                      "B08301_004", 
                      "B08301_010", 
                      "B08301_016", 
                      "B08301_017", 
                      "B08301_018", 
                      "B08301_019", 
                      "B08301_020",
                      "B08301_021")

acs1_vars %>% 
  filter(str_detect(name, "B08301")) %>% 
  view()

all_transit_modes <- get_acs(geography = "tract", 
                             variables = acs1_vars %>%
                               filter(name %in% all_transit_vars) %>%
                               pull(name, label),
                             summary_var = "B08301_001",
                             year = 2019, state = "PA", county = "Allegheny",
                             geometry = F) %>% 
  select(GEOID, variable, estimate, moe, summary_est) %>% 
  mutate(NAME = str_c("Tract", GEOID, sep = " "),
         graph_type = "discrete",
         census_year = 2010) %>% 
  write_csv("inputs/data_sources/commute_modes.csv")
