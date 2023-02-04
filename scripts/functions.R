library(tidyverse)
library(hrbrthemes)
library(here)
library(plotly)

#routing function to get appropriate data source
get_data <- function(x){
  
  file_path <- switch(x,
                      housing = "inputs/data_sources/housing_data.csv",
                      household_income = "inputs/data_sources/median_household_income.csv",
                      commute_modes = "inputs/data_sources/commute_modes.csv",
                      owner_vs_renter = "inputs/data_sources/owner_vs_renter.csv"
  )
  
  read_csv(file_path,
           col_types = list(GEOID = col_character()))
  
}

#routing function to make graphs. graph type depends on how many yerrs are in scope
make_graph <- function(target_df, estimate_var, moe_flag, custom_palette){
  
  graph_type <- target_df %>% 
    distinct(year) %>% 
    count() %>% 
    mutate(graph_type = case_when(n == 1 ~ "single_year",
                                  n > 1 ~ "multiple_year")) %>% 
    pull(graph_type)
  
  switch(graph_type,
         single_year = graph_single_year(target_df, estimate_var, moe_flag, custom_palette),
         multiple_year = graph_multiple_year(target_df, estimate_var, moe_flag, custom_palette)
  )
  
}

#make a graph to show data with only one year in scope
graph_single_year <- function(x, estimate_var, moe_flag, custom_palette){

  #extract variable name
  var_name <- x %>% 
    distinct(variable) %>% 
    pull()
  
  geom_errorbar_switch <- function(x){
    
    switch(x,
           yes = geom_errorbar(aes(xmin = lower_bound, xmax = upper_bound)),
           no = NULL)
    
  }
  
  #print(x)
  unit <- x |> 
    distinct(unit) |> 
    pull()
  
  axis_units <- function(var, unit, axis_type){
    print(var)
    print(unit)
    print(axis_type)
    
    if ((var == "estimate_pct" | unit == "percent") & axis_type == "x"){
      print("x percent")
      scale_x_percent()
      
    } else if ((var == "estimate_pct" | unit == "percent") & axis_type == "y"){
      print("y percent")
      scale_y_percent()
      
    } else if (var == "estimate" & axis_type == "x"){
      print("x comma")
      scale_x_comma()
      
    } else {
      print("y comma")
      scale_y_comma()
      
    }
    
  }
  
  x <- x %>%
    mutate(custom_tooltip = str_c("GEOID: ", GEOID, "\n",
                                  "Year: ", year, "\n",
                                  variable, ": ", estimate,
                                  sep = ""))
  
  #if the data source has a category and margin of error, make a geom_errorbar plot and facet by category
  if (all(c("category", "moe") %in% names(x))) {
    print('type1')
    
    x |> 
      mutate(category = fct_reorder(category, estimate, sum, .desc = TRUE)) |>  
      highlight_key(~GEOID) %>% 
      ggplot(aes(y = GEOID, color = GEOID, customdata = GEOID, text = custom_tooltip)) +
      geom_errorbar_switch(moe_flag) +
      geom_point(aes(x = .data[[estimate_var]]), size = 2) +
      facet_wrap(~category, scales = "free_x") +
      #scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
      axis_units(estimate_var, unit, axis_type = "x") +
      scale_color_manual(values = custom_palette) +
      labs(x = var_name,
           y = NULL) +
      guides(color = "none") +
      theme_bw(base_size = 14)
    
    #if the data source has a category, make a geom_errorbar plot
  } else if ("moe" %in% names(x) & estimate_var == "estimate"){
    print('type2')
    
    x %>% 
      highlight_key(~GEOID) %>% 
      ggplot(aes(y = GEOID, color = GEOID, customdata = GEOID, text = custom_tooltip)) +
      geom_errorbar_switch(moe_flag) +
      geom_point(aes(x = .data[[estimate_var]]), size = 2) +
      #scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
      axis_units(estimate_var, unit, "x") +
      scale_color_manual(values = custom_palette) +
      labs(x = var_name,
           y = NULL) +
      guides(color = "none",
             fill = "none") +
      theme_bw(base_size = 14)
    
    #otherwise make a bar plot
  } else if ("category" %in% names(x)) {
    print('type3')
    
    x %>% 
      highlight_key(~GEOID) %>% 
      ggplot(aes(y = GEOID, fill = GEOID, customdata = GEOID, text = custom_tooltip)) +
      geom_col(aes(x = .data[[estimate_var]]), size = .5, color = "black") +
      #placeholder for scale_x_continuous_switch that deals with comma and pct format depending on estimate_var
      scale_fill_manual(values = custom_palette) +
      facet_wrap(vars(category), scales = "free_x") +
      labs(x = var_name,
           y = NULL) +
      guides(color = "none",
             fill = "none") +
      theme_bw(base_size = 14)
    
  } else {
    print('type4')
    
    x %>% 
      highlight_key(~GEOID) %>% 
      ggplot(aes(y = GEOID, fill = GEOID, customdata = GEOID, text = custom_tooltip)) +
      geom_col(aes(x = .data[[estimate_var]]), size = .5, color = "black") +
      #placeholder for scale_x_continuous_switch that deals with comma and pct format depending on estimate_var
      scale_fill_manual(values = custom_palette) +
      labs(x = var_name,
           y = NULL) +
      guides(color = "none",
             fill = "none") +
      theme_bw(base_size = 14)
    
  }
  
}

#routing function to make graphs when the data source has multiple years in scope
graph_multiple_year <- function(x, estimate_var, moe_flag, custom_palette){

  #extract variable name
  var_name <- x %>% 
    distinct(variable) %>% 
    pull()
  
  #define custom breaks for ggplot
  custom_breaks <- x %>% 
    distinct(year) %>% 
    pull()
  
  geom_ribbon_switch <- function(x){
    
    switch(x,
           yes = geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = .2),
           no = NULL)
  }
  
  x <- x %>%
    mutate(custom_tooltip = str_c("GEOID: ", GEOID, "\n",
                                  "Year: ", year, "\n",
                                  variable, ": ", estimate,
                                  sep = ""))
  
  #if the data source has category and margin of error, make a ribbon plot and facet by category
  if (all(c("category", "moe") %in% names(x))) {
    print("type1")
    
    x %>% 
      mutate(category = fct_reorder(category, estimate, .desc = T)) %>% 
      highlight_key(~GEOID) %>% 
      ggplot(aes(x = year, y = .data[[estimate_var]], color = GEOID, fill = GEOID, group = GEOID, customdata = GEOID, text = custom_tooltip)) +
      geom_ribbon_switch(moe_flag) +
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
    
    #if the data source has category and is in unit terms, make a ribbon plot
  } else if ("moe" %in% names(x)){
    print('type2')
    
    x %>% 
      highlight_key(~GEOID) %>% 
      ggplot(aes(x = year, color = GEOID, fill = GEOID, group = GEOID, customdata = GEOID, text = custom_tooltip)) +
      geom_ribbon_switch(moe_flag) +
      geom_line(aes(y = .data[[estimate_var]]), size = 1) +
      geom_point(aes(y = .data[[estimate_var]]), size = 2) +
      scale_x_continuous(breaks = custom_breaks) +
      scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      scale_color_manual(values = custom_palette) +
      scale_fill_manual(values = custom_palette) +
      labs(x = "Year",
           y = var_name) +
      guides(color = "none",
             fill = "none") +
      theme_bw()
    
    #otherwise make a line graph facted by category
  } else if ("category" %in% names(x)){
    print('type3')
    
    x %>% 
      highlight_key(~GEOID) %>% 
      ggplot(aes(x = year, y = .data[[estimate_var]], color = GEOID, group = GEOID, customdata = GEOID, text = custom_tooltip)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      facet_wrap(vars(category), scales = "free_y") +
      scale_x_continuous(breaks = custom_breaks) +
      #scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      scale_color_manual(values = custom_palette) +
      labs(x = "Year",
           y = var_name) +
      guides(color = "none") +
      theme_bw()
    
    #otherwise make a line graph
  } else {
    print('type4')
    
    x %>% 
      highlight_key(~GEOID) %>% 
      ggplot(aes(x = year, y = .data[[estimate_var]], color = GEOID, group = GEOID, customdata = GEOID, text = custom_tooltip)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = custom_breaks) +
      #scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      scale_color_manual(values = custom_palette) +
      labs(x = "Year",
           y = var_name) +
      guides(color = "none") +
      theme_bw()
    
  }
}