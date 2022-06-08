library(tidyverse)
library(lubridate)
library(janitor)

library(sf)
library(tidycensus)
library(leaflet)
library(RColorBrewer)

tidycensus::census_api_key()

options(scipen = 999, digits = 4, tigris_use_cache = TRUE)

acs_vars <- load_variables(2020, "acs5", cache = TRUE)
dec_vars <- load_variables(year = 2020, dataset = "sf1", cache = T)
acs_vars

ac_geo <- get_acs(geography = "tract", 
                  state = "Pennsylvania",
                  county = "Allegheny County",
                  variables = c(medincome = "B19013_001"),
                  year = 2020,
                  geometry = T) %>% 
  select(GEOID, geometry)

leaflet(ac_geo) %>% 
  addProviderTiles(providers$Stamen.TonerLite) %>% 
  addPolygons(popup = ~GEOID,
              weight = 1,
              highlightOptions = highlightOptions(color = "blue", weight = 2, fillOpacity = 1,
                                                  bringToFront = TRUE))

dormont_df <- tibble(GEOID = c("42003472200",
                               "42003472400",
                               "42003472300",
                               "42003472100"),
                     neighborhood = "Dormont")

neighborhood_df <- ac_geo %>% 
  left_join(dormont_df) %>% 
  replace_na(list(neighborhood = "Other"))

neighborhood_df

neighborhood_df %>% 
  st_write("inputs/allegheny_county_tracts/allegheny_county_tracts.shp")

RColorBrewer::brewer.pal(name = "Set1", n = 3)

neighborhood_pal <- colorFactor(domain = neighborhood_df$neighborhood,
                                palette = "Set1")

neighborhood_df %>% 
  leaflet() %>% 
  addProviderTiles(providers$Stamen.TonerLite) %>% 
  addPolygons(fillColor = ~neighborhood_pal(neighborhood),
              popup = ~GEOID,
              weight = .5,
              fillOpacity = .7,
              highlightOptions = highlightOptions(color = "blue", weight = 2, fillOpacity = 1,
                                                  bringToFront = TRUE))



ac_income <- c(2015:2020) %>% 
  set_names() %>% 
  map_dfr(~{get_acs(geography = "tract", 
                    state = "Pennsylvania",
                    county = "Allegheny County",
                    variables = c(medincome = "B19013_001"),
                    year = .x,
                    geometry = F)},
          .id = "year")

ac_income_2019 <- ac_income %>% 
  filter(year == 2019)

ac_income_2020 <- ac_income %>% 
  filter(year == 2020)

ac_income_2020 %>% 
  distinct(GEOID) %>% 
  anti_join(ac_income_2019 %>% 
              distinct(GEOID))

dormont_income <- ac_income %>% 
  semi_join(dormont_df) %>% 
  left_join(ac_geo) %>% 
  st_sf()

dormont_income %>% 
  ggplot(aes(x = year, y = estimate, color = GEOID, fill = GEOID, group = GEOID)) +
  geom_ribbon(aes(ymin = estimate - moe, ymax = estimate + moe),
              alpha = .3) +
  geom_line() +
  geom_point()


dormont_income %>% 
  ggplot() +
  geom_sf(aes(fill = estimate, alpha = moe)) +
  scale_fill_viridis_c(labels = scales::dollar) +
  scale_alpha_continuous(guide = guide_legend(),
                         range = c(.5, 1)) +
  facet_wrap(~year) +
  theme_void()
