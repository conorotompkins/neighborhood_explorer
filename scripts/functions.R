library(tidyverse)
library(here)

get_housing_data <- function(){
  
  here("inputs/data_sources/housing_data.csv") %>% 
    read_csv(col_types = cols(
      GEOID = col_character(),
      variable = col_character(),
      year = col_double(),
      estimate = col_double(),
      census_year = col_double()
    ))
  
}

# get_housing_data() %>% 
#   distinct(graph_type)

get_median_income <- function(x){
  
  here("inputs/data_sources/median_income.csv") %>% 
    read_csv(col_types = cols(
      GEOID = col_character(),
      year = col_integer(),
      variable = col_character(),
      estimate = col_double(),
      moe = col_double(),
      NAME = col_character(),
      census_year = col_double()
    ))
  
}

get_commute_modes <- function(x){
  
  here("inputs/data_sources/commute_modes.csv") %>% 
    read_csv(col_types = cols(
      GEOID = col_character(),
      NAME = col_character(),
      year = col_integer(),
      variable = col_character(),
      category = col_character(),
      estimate = col_double(),
      moe = col_double(),
      census_year = col_double()
    ))
  
}

# get_median_income() %>% 
#   distinct(graph_type)

data_source <- "housing"

get_data <- function(x){
  
  switch(x,
         housing = get_housing_data(),
         median_income = get_median_income(),
         commute_modes = get_commute_modes()
  )
  
}

#get_data("housing")
#get_data("median_income")

make_graph <- function(target_df){
  
  graph_type <- target_df %>% 
    distinct(year) %>% 
    count() %>% 
    mutate(graph_type = case_when(n == 1 ~ "single_year",
                                  n > 1 ~ "multiple_year")) %>% 
    pull(graph_type)
  
  print(graph_type)
  
  switch(graph_type,
         single_year = graph_single_year(target_df),
         multiple_year = graph_multiple_year(target_df)
  )
  
}

graph_single_year <- function(x){
  
  var_name <- x %>% 
    distinct(variable) %>% 
    pull()
  
  if (all(c("category", "moe") %in% names(x))) {
    
    x %>% 
      ggplot(aes(y = GEOID, customdata = GEOID)) +
      geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe)) +
      geom_point(aes(x = estimate), size = 2) +
      facet_wrap(~category, scales = "free_x") +
      scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
      labs(x = var_name,
           y = NULL) +
      theme_bw(base_size = 14)
    
  } else if ("moe" %in% names(x)){
    
    x %>% 
      mutate(GEOID = fct_reorder(GEOID, estimate)) %>% 
      ggplot(aes(y = GEOID, customdata = GEOID)) +
      geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe)) +
      geom_point(aes(x = estimate), size = 2) +
      scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
      labs(x = var_name,
           y = NULL) +
      theme_bw(base_size = 14)
    
  } else {
    
    x %>% 
      mutate(GEOID = fct_reorder(GEOID, estimate)) %>% 
      ggplot(aes(y = GEOID, customdata = GEOID)) +
      geom_col(aes(x = estimate), size = 2) +
      scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
      labs(x = var_name,
           y = NULL) +
      theme_bw(base_size = 14)
    
  }
  
}

graph_multiple_year <- function(x){
  
  var_name <- x %>% 
    distinct(variable) %>% 
    pull()
  
  custom_breaks <- x %>% 
    distinct(year) %>% 
    pull()
  
  if (all(c("category", "moe") %in% names(x))) {
    
    x %>% 
      mutate(category = fct_reorder(category, estimate, .desc = T)) %>% 
      ggplot(aes(x = year, y = estimate, group = GEOID, customdata = GEOID)) +
      geom_ribbon(aes(ymin = estimate - moe, ymax = estimate + moe), alpha = .3) +
      geom_line() +
      geom_point(size = 1.5) +
      facet_wrap(~category, scales = "free_y") +
      scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      labs(x = "Year",
           y = var_name) +
      theme_bw()
    
  } else if ("moe" %in% names(x)){
    
    x %>% 
      ggplot(aes(x = year, y = estimate, group = GEOID, customdata = GEOID)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = custom_breaks) +
      scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      labs(x = "Year",
           y = var_name) +
      theme_bw()
    
  } else {
    
    x %>% 
      ggplot(aes(x = year, y = estimate, group = GEOID, customdata = GEOID)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = custom_breaks) +
      scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      labs(x = "Year",
           y = var_name) +
      theme_bw()
    
  }
  
}
