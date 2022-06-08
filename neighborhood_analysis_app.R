library(shiny)
library(leaflet)
library(sf)
library(dplyr)

#load shapefile
nc <- st_read(system.file("shape/nc.shp", package="sf")) %>%
  st_transform(4326)

shinyApp(
  ui = fluidPage(
    
    "Update selectize input by clicking on the map",
    
    leafletOutput("map"),
    "I would like the selectize input to update to show all the locations selected,",
    "but also when items are removed here, they are removed on the map too, so linked to the map.",
    selectizeInput(inputId = "selected_locations",
                   label = "selected",
                   choices = nc$NAME,
                   selected = NULL,
                   multiple = TRUE)
  ),
  
  server <- function(input, output, session){
    
    #create empty vector to hold all click ids
    selected_ids <- reactiveValues(ids = vector())
    
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
      updateSelectizeInput(session,
                           inputId = "selected_locations",
                           label = "",
                           choices = nc$NAME,
                           selected = selected$groups)
    })
    
    observeEvent(input$selected_locations, {
      removed_via_selectInput <- setdiff(selected$groups, input$selected_locations)
      added_via_selectInput <- setdiff(input$selected_locations, selected$groups)
      
      if(length(removed_via_selectInput) > 0){
        selected$groups <- input$selected_locations
        proxy %>% hideGroup(group = removed_via_selectInput)
      }
      
      if(length(added_via_selectInput) > 0){
        selected$groups <- input$selected_locations
        proxy %>% showGroup(group = added_via_selectInput)
      }
    }, ignoreNULL = FALSE)
    
  })
