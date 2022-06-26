library(tidyverse)
library(tidycensus)
library(shiny)
library(leaflet)
library(sf)
library(DT)

options(tigris_use_cache = TRUE)

#https://stackoverflow.com/questions/65893124/select-multiple-items-using-map-click-in-leaflet-linked-to-selectizeinput-in

#load shapefile
nc <- st_read(system.file("shape/nc.shp", package="sf")) %>%
  st_transform(4326)

glimpse(nc)

ac_geo <- get_acs(geography = "tract", 
                  state = "Pennsylvania",
                  county = "Allegheny County",
                  variables = c(medincome = "B19013_001"),
                  year = 2020,
                  geometry = T) %>% 
  select(GEOID, geometry) %>% 
  st_transform(4326) %>% 
  mutate(basemap = "basemap")

glimpse(ac_geo)


shinyApp(
  ui = fluidPage(
    
    "Update selectize input by clicking on the map",
    
    leafletOutput("map"),
    DT::dataTableOutput("geoid_table")
  ),
  
  server <- function(input, output, session){
    
    #create empty vector to hold all click ids
    
    #initial map output
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = nc,
                    fillColor = "white",
                    fillOpacity = 0.5,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~NAME,
                    group = "regions",
                    label = ~NAME) %>%
        addPolygons(data = nc,
                    fillColor = "red",
                    fillOpacity = 0.5,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~CNTY_ID,
                    group = ~NAME) %>%
        hideGroup(group = nc$NAME) # nc$CNTY_ID
    }) #END RENDER LEAFLET
    
    #define leaflet proxy for second regional level map
    proxy <- leafletProxy("map")
    
    #create empty vector to hold all click ids
    selected <- reactiveValues(groups = vector())
    
    observeEvent(input$map_shape_click, {
      if(input$map_shape_click$group == "regions"){
        selected$groups <- c(selected$groups, input$map_shape_click$id)
        proxy %>% showGroup(group = input$map_shape_click$id)
      } else {
        selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
        proxy %>% hideGroup(group = input$map_shape_click$group)
      }
      print(selected$groups)
    })
    
    geoid_table <- reactive({
      
      selected$groups %>% 
        enframe()
      
    })
    
    output$geoid_table <- DT::renderDataTable({
      
      geoid_table()
      
    })
    
  })
