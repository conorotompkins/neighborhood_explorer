library(tidyverse)
library(tidycensus)
library(shiny)
library(leaflet)
library(sf)
library(DT)

library(here)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

source(here("scripts/functions.R"))

#https://stackoverflow.com/questions/65893124/select-multiple-items-using-map-click-in-leaflet-linked-to-selectizeinput-in

#load shapefile
ac_geo <- st_read("inputs/allegheny_county_tract_history/allegheny_county_tract_history.shp") %>% 
  rename(census_year = cnss_yr) %>% 
  mutate(NAME = str_c("Tract", GEOID, sep = " ")) #needs to be different than only GEOID value

ui <- fluidPage(
  
  column(width = 2,
         
         fluidRow(
           
           selectizeInput(inputId = "data_source",
                          label = "Choose topic",
                          choices = c("median_income", "housing"))
         ),
  ),
  
  column(width = 10,
         
         fluidRow(
           
           leafletOutput("map")
           
         ),
         
         fluidRow(
           
           column(width = 6,
                  
                  DT::dataTableOutput("geoid_table")
                  
           ),
           
           column(width = 6,
                  
                  plotOutput("bar_chart")
           )
         )
         
  )
)

server <- function(input, output, session){
  
  data_source_reactive <- reactive({
    
    get_data(input$data_source)
    
  })
  
  ac_tracts_reactive <- reactive({
    
    target_year <- data_source_reactive() %>% 
      distinct(census_year) %>% 
      pull()
    
    ac_geo %>% 
      filter(census_year == target_year)
    
  })
  
  #create empty vector to hold all click ids
  
  #initial map output
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      #basemap
      addPolygons(data = ac_tracts_reactive(),
                  fillColor = "white",
                  fillOpacity = 0.5,
                  color = "black",
                  stroke = TRUE,
                  weight = 1,
                  layerId = ~NAME,
                  group = "regions",
                  label = ~NAME) %>%
      #selected polygons
      addPolygons(data = ac_tracts_reactive(),
                  fillColor = "red",
                  fillOpacity = 0.5,
                  weight = 1,
                  color = "black",
                  stroke = TRUE,
                  layerId = ~GEOID,
                  group = ~GEOID) %>%
      #hide selected polygons at start
      hideGroup(group = ac_tracts_reactive()$GEOID)
  }) #END RENDER LEAFLET
  
  #define leaflet proxy for second regional level map
  proxy <- leafletProxy("map")
  
  #create empty vector to hold all click ids
  selected <- reactiveValues(groups = vector())
  
  #reset selected tracts when data source changes
  #eventually needs to only change when census tract year changes
  observeEvent(input$data_source, {
    
    selected$groups <- NULL
    
  })
  
  observeEvent(input$map_shape_click, {
    
    if(input$map_shape_click$group == "regions"){
      selected$groups <- c(selected$groups, str_remove(input$map_shape_click$id, "^Tract ")) #remove "Tract " from start of id on the fly
      proxy %>% showGroup(group = str_remove(input$map_shape_click$id, "^Tract "))
    } else {
      selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
      proxy %>% hideGroup(group = input$map_shape_click$group)
    }
    # print(selected$groups)
  }, ignoreInit = TRUE)
  
  geoid_table_reactive <- reactive({
    
    req(length(selected$groups) > 0)
    
    selected$groups %>% 
      enframe(value = "GEOID") %>% 
      select(-name) %>% 
      left_join(st_drop_geometry(ac_tracts_reactive()), by = "GEOID") %>% 
      select(NAME, GEOID) %>% 
      left_join(get_data(input$data_source))
    
  })
  
  output$geoid_table <- DT::renderDataTable({
    
    req(geoid_table_reactive())
    
    # geoid_table_reactive() %>% 
    #   distinct(graph_type) %>% 
    #   pull() %>% 
    #   print()
    
    geoid_table_reactive()
    
  })
  
  
  
  output$bar_chart <- renderPlot({
    
    req(geoid_table_reactive)
    
    geoid_table_reactive() %>% 
      distinct(graph_type) %>% 
      pull() %>% 
      make_graph(geoid_table_reactive())
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
