library(tidyverse)
library(here)

get_housing_data <- function(){
  
  here("inputs/data_sources/housing_data.csv") %>% 
    read_csv(col_types = cols(
      GEOID = col_character(),
      year = col_double(),
      housing_units = col_double()
    ))
  
}

#get_housing_data()

get_median_income <- function(x){
  
  here("inputs/data_sources/median_income.csv") %>% 
    read_csv(col_types = cols(
      GEOID = col_character(),
      variable = col_character(),
      estimate = col_double(),
      moe = col_double(),
      NAME = col_character()
    ))
  
}

#get_median_income()

data_source <- "housing"

get_data <- function(x){
  
  switch(x,
         housing = get_housing_data(),
         median_income = get_median_income()
  )
  
}

#get_data("housing")
#get_data("median_income")

