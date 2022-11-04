library(tidyverse)
library(here)
library(plotly)

get_housing_data <- function(){
  
  here("inputs/data_sources/housing_data.csv") %>% 
    read_csv(col_types = cols(
      GEOID = col_character(),
      variable = col_character(),
      year = col_double(),
      estimate = col_double(),
      tract_year = col_double(),
      unit = col_character()
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
      tract_year = col_double(),
      unit = col_character()
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
      tract_year = col_double(),
      unit = col_character()
    ))
  
}

get_owner_vs_renter_population <- function(x){
  
  here("inputs/data_sources/owner_vs_renter.csv") %>% 
    read_csv(col_types = cols(
      GEOID = col_character(),
      NAME = col_character(),
      year = col_integer(),
      variable = col_character(),
      estimate = col_double(),
      moe = col_double(),
      tract_year = col_double(),
      unit = col_character()
    ))
  
}

# get_median_income() %>% 
#   distinct(graph_type)

data_source <- "housing"

get_data <- function(x){
  
  switch(x,
         housing = get_housing_data(),
         median_income = get_median_income(),
         commute_modes = get_commute_modes(),
         owner_vs_renter = get_owner_vs_renter_population()
  )
  
}

#get_data("housing")
#get_data("median_income")

make_graph <- function(target_df, custom_palette){
  
  graph_type <- target_df %>% 
    distinct(year) %>% 
    count() %>% 
    mutate(graph_type = case_when(n == 1 ~ "single_year",
                                  n > 1 ~ "multiple_year")) %>% 
    pull(graph_type)
  
  switch(graph_type,
         single_year = graph_single_year(target_df, custom_palette),
         multiple_year = graph_multiple_year(target_df, custom_palette)
  )
  
}

graph_single_year <- function(x, custom_palette){
  
  var_name <- x %>% 
    distinct(variable) %>% 
    pull()
  
  if (all(c("category", "moe") %in% names(x))) {
    
    x %>% 
      mutate(category = fct_reorder(category, estimate, .desc = T)) %>% 
      highlight_key(~GEOID) %>% 
      ggplot(aes(y = GEOID, color = GEOID, customdata = GEOID)) +
      geom_errorbar(aes(xmin = lower_bound, xmax = upper_bound)) +
      geom_point(aes(x = estimate), size = 2) +
      facet_wrap(~category, scales = "free_x") +
      scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
      scale_color_manual(values = custom_palette) +
      labs(x = var_name,
           y = NULL) +
      guides(color = "none") +
      theme_bw(base_size = 14)
    
  } else if ("moe" %in% names(x)){
    
    x %>% 
      mutate(GEOID = fct_reorder(GEOID, estimate)) %>% 
      highlight_key(~GEOID) %>% 
      ggplot(aes(y = GEOID, color = GEOID, customdata = GEOID)) +
      geom_errorbar(aes(xmin = lower_bound, xmax = upper_bound)) +
      geom_point(aes(x = estimate), size = 2) +
      scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
      scale_color_manual(values = custom_palette) +
      labs(x = var_name,
           y = NULL) +
      guides(color = "none",
             fill = "none") +
      theme_bw(base_size = 14)
    
  } else {
    
    x %>% 
      mutate(GEOID = fct_reorder(GEOID, estimate)) %>% 
      highlight_key(~GEOID) %>% 
      ggplot(aes(y = GEOID, fill = GEOID, customdata = GEOID)) +
      geom_col(aes(x = estimate), size = .5, color = "black") +
      scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
      scale_fill_manual(values = custom_palette) +
      labs(x = var_name,
           y = NULL) +
      guides(color = "none",
             fill = "none") +
      theme_bw(base_size = 14)
    
  }
  
}

graph_multiple_year <- function(x, custom_palette){
  
  var_name <- x %>% 
    distinct(variable) %>% 
    pull()
  
  custom_breaks <- x %>% 
    distinct(year) %>% 
    pull()
  
  if (all(c("category", "moe") %in% names(x))) {
    
    x %>% 
      mutate(category = fct_reorder(category, estimate, .desc = T)) %>% 
      highlight_key(~GEOID) %>% 
      ggplot(aes(x = year, y = estimate, color = GEOID, fill = GEOID, group = GEOID, customdata = GEOID)) +
      geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = .2) +
      geom_line() +
      geom_point(size = 1.5) +
      facet_wrap(~category, scales = "free_y") +
      scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      scale_color_manual(values = custom_palette) +
      scale_fill_manual(values = custom_palette) +
      labs(x = "Year",
           y = var_name) +
      guides(color = "none",
             fill = "none") +
      theme_bw()
    
  } else if ("moe" %in% names(x)){
    
    x %>% 
      highlight_key(~GEOID) %>% 
      ggplot(aes(x = year, color = GEOID, fill = GEOID, group = GEOID, customdata = GEOID)) +
      geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = .2) +
      geom_line(aes(y = estimate), size = 1) +
      geom_point(aes(y = estimate), size = 2) +
      scale_x_continuous(breaks = custom_breaks) +
      scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      scale_color_manual(values = custom_palette) +
      scale_fill_manual(values = custom_palette) +
      labs(x = "Year",
           y = var_name) +
      guides(color = "none",
             fill = "none") +
      theme_bw()
    
  } else {
    
    x %>% 
      highlight_key(~GEOID) %>% 
      ggplot(aes(x = year, y = estimate, color = GEOID, group = GEOID, customdata = GEOID)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = custom_breaks) +
      scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      scale_color_manual(values = custom_palette) +
      labs(x = "Year",
           y = var_name) +
      guides(color = "none") +
      theme_bw()
    
  }
  
}
