library(tidyverse)
library(tidycensus)
library(shiny)
library(leaflet)
library(sf)
library(DT)

options(tigris_use_cache = TRUE)

#https://stackoverflow.com/questions/65893124/select-multiple-items-using-map-click-in-leaflet-linked-to-selectizeinput-in

#load shapefile
ac_geo <- get_acs(geography = "tract", 
                  state = "Pennsylvania",
                  county = "Allegheny County",
                  variables = c(medincome = "B19013_001"),
                  year = 2020,
                  geometry = T) %>% 
  select(GEOID, geometry, variable, estimate, moe) %>% 
  st_transform(4326) %>% 
  mutate(NAME = str_c("Tract", GEOID, sep = " ")) #needs to be different than only GEOID value

glimpse(ac_geo)

shinyApp(
  ui = fluidPage(
    
    fluidRow(
      
      leafletOutput("map")
    ),
    
    fluidRow(
      column(width = 4,
        DT::dataTableOutput("geoid_table")
      ),
      column(width = 7,
        plotOutput("bar_chart")
      )
    )
  ),
  
  server <- function(input, output, session){
    
    #create empty vector to hold all click ids
    
    #initial map output
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        #basemap
        addPolygons(data = ac_geo,
                    fillColor = "white",
                    fillOpacity = 0.5,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~NAME,
                    group = "regions",
                    label = ~NAME) %>%
        #selected polygons
        addPolygons(data = ac_geo,
                    fillColor = "red",
                    fillOpacity = 0.5,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~GEOID,
                    group = ~GEOID) %>%
        #hide selected polygons at start
        hideGroup(group = ac_geo$GEOID)
    }) #END RENDER LEAFLET
    
    #define leaflet proxy for second regional level map
    proxy <- leafletProxy("map")
    
    #create empty vector to hold all click ids
    selected <- reactiveValues(groups = vector())
    
    observeEvent(input$map_shape_click, {
      if(input$map_shape_click$group == "regions"){
        selected$groups <- c(selected$groups, str_remove(input$map_shape_click$id, "^Tract ")) #remove "Tract " from start of id on the fly
        proxy %>% showGroup(group = str_remove(input$map_shape_click$id, "^Tract "))
      } else {
        selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
        proxy %>% hideGroup(group = input$map_shape_click$group)
      }
      print(selected$groups)
    })
    
    geoid_table <- reactive({
      
      req(length(selected$groups) > 0)
      
      selected$groups %>% 
        enframe(value = "GEOID") %>% 
        select(-name) %>% 
        left_join(st_drop_geometry(ac_geo), by = "GEOID") %>% 
        select(NAME, GEOID, variable, estimate, moe) %>% 
        mutate(GEOID = fct_reorder(GEOID, estimate))
      
    })
    
    output$geoid_table <- DT::renderDataTable({
      
      req(geoid_table())
      
      geoid_table()
      
    })
    
    output$bar_chart <- renderPlot({
      
      req(geoid_table)
      
      geoid_table() %>% 
        ggplot(aes(estimate, GEOID)) +
        geom_col() +
        theme_bw()
      
    })
    
  })
