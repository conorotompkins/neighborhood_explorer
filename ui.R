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
    
  ),
  
  # div(
  #   tags$ul(
  #     tags$li(
  #       h1(tags$a(style = "color: #2B2B2B", href = "https://ctompkins.netlify.app/", "Conor Tompkins"))
  #     ),
  #     tags$li(
  #       h2(
  #         a(href = "https://ctompkins.netlify.app/#posts", "posts")
  #       )
  #     )
  #   )
  # ),
  
  fluidRow(
    
    column(width = 2,
           
           #user selects topic
           selectizeInput(inputId = "data_source",
                          label = "Choose topic",
                          choices = c("median_income", "housing", "commute_modes", "owner_vs_renter"))
           
    ),
    
    column(width = 3,
           
           #user selects years in scope
           sliderInput(inputId = "year_slider",
                       label = "Year",
                       value = c(2010, 2019),
                       min = 2010,
                       max = 2019,
                       sep = "")
    ),
    
    column(width = 3,
           
           #user can select which categories are in scope
           uiOutput("category_filter")
           
    )
  ),
  
  column(width = 10,
         
         fluidRow(
           
           #shows leaflet map
           leafletOutput("map")
           
         ),
         
         fluidRow(
           
           tabsetPanel(
             
             #shows graph
             tabPanel(title = "Graph",
                      plotlyOutput("bar_chart")),
             
             #shows summary table
             tabPanel(title = "Table",
                      DT::dataTableOutput("geoid_table"))
             
           )
           
         )
  )
  
)