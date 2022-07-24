library(tidyverse)
library(here)

get_housing_data <- function(){
  
  here("inputs/data_sources/housing_data.csv") %>% 
    read_csv(col_types = cols(
      GEOID = col_character(),
      variable = col_character(),
      year = col_double(),
      estimate = col_double(),
      graph_type = col_character(),
      census_year = col_double()
    ))
  
}

# get_housing_data() %>% 
#   distinct(graph_type)

get_median_income <- function(x){
  
  here("inputs/data_sources/median_income.csv") %>% 
    read_csv(col_types = cols(
      GEOID = col_character(),
      variable = col_character(),
      estimate = col_double(),
      moe = col_double(),
      NAME = col_character(),
      graph_type = col_character(),
      census_year = col_double()
    ))
  
}

# get_median_income() %>% 
#   distinct(graph_type)

data_source <- "housing"

get_data <- function(x){
  
  switch(x,
         housing = get_housing_data(),
         median_income = get_median_income()
  )
  
}

#get_data("housing")
#get_data("median_income")

make_graph <- function(graph_type, target_df){
  
  switch(graph_type,
         point_in_time = graph_point_in_time(target_df),
         time_series = graph_time_series(target_df)
  )
  
}

graph_point_in_time <- function(x){
  
  var_name <- x %>% 
    distinct(variable) %>% 
    pull()
  
  x %>% 
    ggplot(aes(y = GEOID)) +
    geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe)) +
    geom_point(aes(x = estimate), size = 2) +
    scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
    labs(x = var_name,
         y = NULL) +
    theme_bw(base_size = 14)
  
}

graph_time_series <- function(x){
  
  x %>% 
    ggplot(aes(x = year, y = estimate)) +
    geom_line(aes(group = GEOID), size = 2) +
    theme_bw()
  
}