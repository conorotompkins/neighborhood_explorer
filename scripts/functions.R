library(tidyverse)
library(here)
library(plotly)

#routing function to get appropriate data source
get_data <- function(x){
  
  file_path <- switch(x,
         housing = "inputs/data_sources/housing_data.csv",
         median_income = "inputs/data_sources/median_income.csv",
         commute_modes = "inputs/data_sources/commute_modes.csv",
         owner_vs_renter = "inputs/data_sources/owner_vs_renter.csv"
  )
  
  read_csv(file_path,
           col_types = list(GEOID = col_character()))
  
}

#routing function to make graphs. graph type depends on how many yerrs are in scope
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

#make a graph to show data with only one year in scope
graph_single_year <- function(x, custom_palette){
  
  #extract variable name
  var_name <- x %>% 
    distinct(variable) %>% 
    pull()
  
  #if the data source has a category and margin of error, make a geom_errorbar plot and facet by category
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
    
  #if the data source has a category, make a geom_errorbar plot
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
    
    #otherwise make a bar plot
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

#routing function to make graphs when the data source has multiple years in scope
graph_multiple_year <- function(x, custom_palette){
  
  #extract variable name
  var_name <- x %>% 
    distinct(variable) %>% 
    pull()
  
  #define custom breaks for ggplot
  custom_breaks <- x %>% 
    distinct(year) %>% 
    pull()
  
  #if the data source has category and margin of error, make a ribbon plot and facet by category
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
    
  #if the data source has category, make a ribbon plot
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
    
  #otherwise make a line graph
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