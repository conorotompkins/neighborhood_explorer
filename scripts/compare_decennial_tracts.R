library(tidyverse)
library(lubridate)
library(janitor)

library(sf)
library(tidycensus)
library(tigris)
library(gganimate)

options(scipen = 999, digits = 4, tigris_use_cache = TRUE)

ac_geo_2010 <- get_acs(geography = "tract", 
                  state = "Pennsylvania",
                  county = "Allegheny County",
                  variables = c(medincome = "B19013_001"),
                  year = 2010,
                  geometry = T) %>% 
  mutate(geoid_year = 2010) %>% 
  select(GEOID, geoid_year, geometry) %>% 
  erase_water()

ac_geo_2020 <- get_acs(geography = "tract", 
                       state = "Pennsylvania",
                       county = "Allegheny County",
                       variables = c(medincome = "B19013_001"),
                       year = 2020,
                       geometry = T) %>% 
  mutate(geoid_year = 2020) %>% 
  select(GEOID, geoid_year, geometry) %>% 
  erase_water()

ac_geo_2010 %>% 
  ggplot() +
  geom_sf()

ac_geo_2020 %>% 
  ggplot() +
  geom_sf()

tract_changes <- ac_geo_2010 %>% 
  bind_rows(ac_geo_2020) %>% 
  ggplot() +
  geom_sf() +
  labs(title = "Allegheny County Decennial Census Tracts",
       subtitle = "Census year {current_frame}") +
  transition_manual(geoid_year) +
  theme_void(base_size = 20)

tract_changes_anim <- animate(tract_changes, nframes = 200, fps = 20,
                              width = 1000, height = 1000)

tract_changes_anim

anim_save(filename = "tract_changes_anim.gif", anim = tract_changes_anim)

ac_geo_2010 %>% 
  st_drop_geometry() %>% 
  select(geoid_year, GEOID) %>% 
  anti_join(ac_geo_2020 %>% 
              st_drop_geometry() %>% 
              select(geoid_year, GEOID))
