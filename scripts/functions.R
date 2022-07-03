library(tidyverse)
library(here)

get_housing_data <- function(){
  
  here("inputs/data_sources/housing_data.csv") %>% 
    read_csv()
  
}

get_housing_data()

get_median_income <- function(x){
  
  
  
}

data_source <- "housing"

get_data <- function(x){
  
  switch(x,
         housing = get_housing_data(),
         median_income = get_median_income())

  
  
}

get_data("housing")
