library(tidyverse)
library(tidycensus)
library(shiny)
library(leaflet)
library(sf)
library(DT)
library(plotly)
library(scales)
library(here)

ui <- fluidPage(
  
  fluidRow(
    #define app header
    div(#define style that lays out subdivs horizontally and assigns percentages to width
      
      style = "height:100%; max-width:350px; overflow: hidden; padding: 10px; vertical-align: bottom",
      div(style = "float: left; width:230px; padding: 5px",
          span(
            a(style = "font-size: 25px; font-family: roboto, sans-serif; font-weight: 700; color: #2B2B2B; text-decoration: none;",
              href = "https://ctompkins.netlify.app/", "Conor Tompkins")
          )
      ),
      div(style = "float: left; width:50px; padding: 5px",
          span(
            a(style = "font-size: 16px; font-family: roboto, sans-serif; font-weight: 400; color: #2B2B2B; text-decoration: none;",
              href = "https://ctompkins.netlify.app/#posts", "Posts")
          )
      ),
      div(style = "float: left; width:50px; padding: 5px",
          span(
            a(style = "font-size: 16px; font-family: roboto, sans-serif; font-weight: 400; color: #2B2B2B; text-decoration: none;",
              href = "https://ctompkins.netlify.app/apps/app-gallery/", "Apps")
          )
      )
      
    )
  ),
  
  fluidRow(
    
    column(width = 3,
           
           #user selects topic
           selectizeInput(inputId = "data_source",
                          label = "Choose topic",
                          choices = c("Household Income" = "household_income", 
                                      "Housing Units" = "housing", 
                                      "Commute Mode" = "commute_modes", 
                                      "Home Ownership" = "owner_vs_renter")),
           
           #user selects years in scope
           sliderInput(inputId = "year_slider",
                       label = "Year",
                       value = c(2010, 2019),
                       min = 2010,
                       max = 2019,
                       sep = ""),
           
           #user can select which categories are in scope
           uiOutput("category_filter"),
           
           uiOutput("pct_toggle"),
           
           uiOutput("toggle_moe")
           
    ),
    
    column(width = 9,
           
           fluidRow(
             
             #shows leaflet map
             leafletOutput("map",
                           height = "300px")
             
           ),
           
           fluidRow(
             
             tabsetPanel(
               
               #shows graph
               tabPanel(title = "Graph",
                        plotlyOutput("plotly_graph")),
               
               #shows summary table
               tabPanel(title = "Table",
                        DT::dataTableOutput("summary_table"))
               
             )
             
           )
    )
  )
)