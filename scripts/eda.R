library(tidyverse)
library(sf)
library(hrbrthemes)
library(gganimate)

theme_set(theme_ipsum())

#https://osf.io/fzv5e/
#https://www.nature.com/articles/s41597-022-01184-x#Sec10
#https://twitter.com/snmarkley/status/1502234165521092610

housing_df <- st_read("data/HHUUD10.gdb") %>% 
  rename(geometry = Shape) %>% 
  mutate(UY1 = na_if(UY1, 2035),
         UY2 = na_if(UY2, 2035))

glimpse(housing_df)

housing_df %>% 
  st_drop_geometry() %>% 
  distinct(GEOID10, UY1) %>% 
  ggplot(aes(UY1)) +
  geom_histogram()

housing_df %>% 
  st_drop_geometry() %>% 
  distinct(STATE) %>% 
  view()

housing_df %>% 
  filter(STATE == "PA",
         COUNTY == "Allegheny") %>% 
  mutate(valid = st_is_valid(geometry)) %>% 
  st_drop_geometry() %>% 
  count(valid)


ac_housing <- housing_df %>% 
  filter(STATE == "PA",
         COUNTY == "Allegheny") %>% 
  select(STATE, COUNTY, GEOID10, UY1, UY2, starts_with("hu"), starts_with("sqmi"), starts_with("pdev")) %>% 
  tigris::erase_water()

best_crs <- ac_housing %>% 
  crsuggest::suggest_crs() %>% 
  slice_head(n = 1) %>% 
  select(crs_code) %>% 
  pull() %>% 
  as.numeric()

ac_housing <- ac_housing %>% 
  st_transform(crs = best_crs)

ac_housing_hu <- ac_housing %>% 
  select(-starts_with("pdev")) %>% 
  pivot_longer(cols = starts_with("hu"), names_to = "year", values_to = "housing_units")

year_lookup <- ac_housing_hu %>% 
  st_drop_geometry() %>% 
  distinct(year) %>% 
  mutate(year_fixed = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2019))

ac_housing_hu <- ac_housing_hu %>% 
  #st_drop_geometry() %>% 
  left_join(year_lookup) %>% 
  select(-year) %>% 
  rename(year = year_fixed)

ac_housing_hu %>% 
  st_drop_geometry() %>% 
  count(GEOID10) %>% 
  distinct(n)

ac_housing_hu %>% 
  filter(year == 2019) %>% 
  #tigris::erase_water() %>% 
  ggplot() +
  geom_sf()

ac_housing_hu %>% 
  st_drop_geometry() %>% 
  group_by(year) %>% 
  summarize(housing_units = sum(housing_units)) %>% 
  ungroup() %>% 
  mutate(diff = housing_units - lag(housing_units)) %>% 
  pivot_longer(cols = c(housing_units, diff), names_to = "measure", values_to = "metric") %>% 
  mutate(year = fct_inorder(as.character(year)),
         measure = factor(measure, levels = c("housing_units", "diff"), labels = c("Total Units", "Change in Units"))) %>%  
  ggplot(aes(year, metric, group = 1)) +
  geom_line() +
  geom_point() +
  facet_wrap(~measure, ncol = 1, scales = "free_y") +
  scale_y_comma() +
  labs(x = "Year",
       y  = "Housing Units")

ac_housing_hu %>% 
  mutate(year = as.character(year) %>% fct_inorder()) %>% 
  ggplot(aes(year, housing_units, group = GEOID10)) +
  geom_line(alpha = .3) +
  theme_bw()

hu_diff <- ac_housing_hu %>% 
  group_by(GEOID10) %>% 
  filter(year == min(year) | year == max(year)) %>% 
  ungroup() %>% 
  select(GEOID10, year, housing_units) %>% 
  pivot_wider(names_from = year, values_from = housing_units) %>% 
  mutate(diff = `2019` - `1940`) %>% 
  st_as_sf()

hu_diff %>% 
  ggplot(aes(diff)) +
  geom_histogram()

theme_map <- theme_ipsum() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

hu_diff %>% 
  ggplot() +
  geom_sf(aes(fill = diff), color = NA) +
  scale_fill_viridis_c() +
  labs(title = "Housing Unit Change from 1940-2019",
       fill = "Difference") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

hu_diff %>% 
  pivot_longer(cols = c(`1940`, `2019`), names_to = "year", values_to = "housing_units") %>% 
  #mutate(GEOID10 = fct_inorder(GEOID10, diff)) %>% 
  ggplot(aes(year, housing_units, group = GEOID10)) +
  geom_line(alpha = .3) +
  geom_point() +
  #scale_alpha_continuous(range = c(.1, .8)) +
  transition_reveal(diff) +
  labs(title = "Housing Unit Change from 1940-2019",
       x = "Year",
       y = "Housing Units",
       alpha = "Difference") +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank())

tract_order_1940 <- ac_housing_hu %>% 
  filter(year == 1940) %>% 
  arrange(housing_units) %>% 
  st_drop_geometry() %>% 
  pull(GEOID10)

ac_housing_hu %>% 
  mutate(year = as.character(year) %>% fct_inorder(),
         GEOID10 = fct_relevel(GEOID10, tract_order_1940)) %>% 
  ggplot(aes(year, GEOID10, fill = housing_units)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme(axis.text.y = element_blank())

ac_hu_map <- ac_housing_hu %>% 
  #filter(year == 1940) %>% 
  ggplot() +
  geom_sf(aes(fill = housing_units), color = NA) +
  scale_fill_viridis_c("Housing Units") +
  facet_wrap(~year) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ac_hu_map

ggsave("output/ac_hu_map.png", ac_hu_map, width = 12, height = 12, dpi = 300)


#try with pdev vars

ac_dev <- ac_housing %>% 
  select(GEOID10, starts_with("pdev")) %>% 
  pivot_longer(cols = starts_with("pdev"), names_to = "year", values_to = "pct_dev") 

dev_years <- ac_dev %>% 
  st_drop_geometry() %>% 
  distinct(year) %>% 
  mutate(year_fixed = c(1992, 2001, 2011))

ac_dev <- ac_dev %>% 
  left_join(dev_years) %>% 
  select(-year) %>% 
  rename(year = year_fixed)

ac_dev %>% 
  ggplot() +
  geom_sf(aes(fill = pct_dev), color = NA) +
  facet_wrap(~year) +
  scale_fill_viridis_c() +
  labs(title = "Percent of land that is developed",
       fill = "%") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

#show hu vs distance to downtown over time


# density

ac_sqmi <- ac_housing %>% 
  select(GEOID10, starts_with("sqmi")) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  pivot_longer(starts_with("sqmi"), names_to = "year", values_to = "sqmi")

ac_sqmi_year <- ac_sqmi %>% 
  distinct(year) %>% 
  mutate(year_fixed = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2019))

ac_sqmi <- ac_sqmi %>% 
  left_join(ac_sqmi_year) %>% 
  select(-year) %>% 
  rename(year = year_fixed)

ac_density <- ac_housing_hu %>% 
  select(GEOID10, year, housing_units) %>% 
  left_join(ac_sqmi) %>% 
  mutate(density = housing_units / sqmi,
         density = round(density, 0) / 1000,
         year = as.factor(year))

levels(ac_density$year)

ac_density %>% 
  st_drop_geometry() %>% 
  view()

histogram_anim <- ac_density %>% 
  st_drop_geometry() %>% 
  select(GEOID10, year, density) %>% 
  mutate(#density = cut_width(density, width = 1, boundary = .5)
    density = round(density, 0)) %>% 
  count(year, density) %>% 
  ggplot(aes(density, n, fill = density)) +
  geom_col() +
  scale_fill_viridis_c() +
  transition_states(year) +
  labs(title = "More housing units created in suburban areas",
       subtitle = "{closest_state}",
       x = "Density (units per sq. mile)",
       y = "Count of census tracts",
       fill = "Density")

histogram_anim <- animate(histogram_anim)

histogram_anim

histogram_anim_old <- ac_density %>% 
  st_drop_geometry() %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(density)) +
  geom_histogram(bins = 60) +
  transition_states(year) +
  labs(title = "More housing units created in suburban areas",
       subtitle = "{closest_state}",
       x = "Density (units per sq. mile)",
       y = "Count of census tracts")

histogram_anim_old <- animate(histogram_anim_old)
histogram_anim_old

scatter_plot_anim <- ac_density %>% 
  ggplot(aes(sqmi, housing_units, color = density)) +
  geom_point() +
  scale_color_viridis_c() +
  transition_states(year) +
  labs(title = "Suburbanization of Allegheny County",
       subtitle = "Density in {closest_state}",
       x = "Sq. miles",
       y = "Housing Units",
       color = "Density")

scatter_plot_anim <- animate(scatter_plot_anim)

scatter_plot_anim

ac_density %>% 
  group_by(GEOID10) %>% 
  filter(density == max(density)) %>% 
  ungroup() %>% 
  rename(max_density_year = year) %>% 
  st_drop_geometry() %>% 
  view()

ac_density %>% 
  group_by(GEOID10) %>% 
  filter(density == max(density)) %>% 
  ungroup() %>% 
  rename(max_density_year = year) %>% 
  st_transform(crs = best_crs) %>% 
  ggplot() +
  geom_sf(aes(fill = max_density_year), color = NA) +
  scale_fill_viridis_d(direction = -1) +
  labs(title = "Year of peak housing density",
       fill = "Year") +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())




