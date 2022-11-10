library(tidyverse)
library(tidycensus)
library(shiny)
library(leaflet)
library(sf)
library(DT)
library(plotly)
library(scales)
library(here)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

source(here("scripts/functions.R"))

#https://stackoverflow.com/questions/65893124/select-multiple-items-using-map-click-in-leaflet-linked-to-selectizeinput-in

#load shapefile
ac_geo <- st_read("inputs/allegheny_county_tract_history/allegheny_county_tract_history.shp") %>%
  mutate(NAME = str_c("Tract", GEOID, sep = " ")) #needs to be different than only GEOID value

server <- function(input, output, session){
  
  #prompt the user with a modal at start
  observeEvent(1, {
    
    showModal(
      
      modalDialog(
        
        title = "Tip",
        "Click on census tracts to start!"
        
      )
    )
    
  })
  
  #fetch data
  data_source_reactive <- reactive({
    
    get_data(input$data_source)
    
  })
  
  #dynamically update SliderInput based on the data source. some data sources have yearly data, others have it by decade
  observeEvent(data_source_reactive(), {
    
    year_min <- min(data_source_reactive()$year)
    year_max <- max(data_source_reactive()$year)
    
    year_step <- ifelse(year_min == 1940, 10, 1)
    
    updateSliderInput(inputId = "year_slider",
                      value = c(year_min, year_max),
                      min = year_min,
                      max = year_max,
                      step = year_step)
    
  })
  
  #tracts which decade the census tracts were created in
  tract_year_reactive <- reactive({
    
    isolate(data_source_reactive()) %>% 
      distinct(tract_year) %>% 
      pull()
    
  })
  
  #filters simple features df with the correct census tract year
  ac_tracts_reactive <- reactive({
    
    ac_geo %>% 
      filter(tract_year == tract_year_reactive())
    
  })
  
  #create empty vector to hold all census tracts the user clicks on
  selected <- reactiveValues(groups = vector())
  
  #reset selected tracts when tract_year_reactive changes
  observeEvent(tract_year_reactive(), {
    
    selected$groups <- NULL
    
  })
  
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
                  layerId = ~NAME, #NAME = str_c("Tract", GEOID, sep = " ")
                  group = "base_map",
                  label = ~NAME)
  }) #END RENDER LEAFLET
  
  #define leaflet proxy for second regional level map
  proxy <- leafletProxy("map")
  
  #df that only contains data for the tracts the user has selected
  selected_tracts_geo_reactive <- reactive({
    
    ac_tracts_reactive() %>% 
      filter(GEOID %in% selected$groups)
    
  })
  
  #set up palette
  tract_count <- reactive({
    
    length(selected$groups)
    
  })
  
  palette_reactive <- reactive({
    
    hue_pal()(tract_count())
    
  })
  
  #define logic for how to accumulate tracts based on user clicks
  observeEvent(input$map_shape_click, {
    
    if(input$map_shape_click$group == "base_map"){
      #when the user clicks a polygon on the basemap, add that polygon to selected$groups and display the new second layer
      selected$groups <- c(selected$groups, str_remove(input$map_shape_click$id, "^Tract ")) #remove "Tract " from start of id on the fly
      
      leaflet_pal <- colorFactor(palette_reactive(), selected_tracts_geo_reactive()$GEOID)
      
      proxy %>%
        #selected polygons
        addPolygons(data = selected_tracts_geo_reactive(),
                    fillColor = ~leaflet_pal(GEOID),
                    fillOpacity = .5,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~GEOID, #feeds into later setdiff function to remove a tract
                    group = ~GEOID,
                    label = ~GEOID)
    } else if(input$map_shape_click$group == "hover_polygon") {
      #when the user clicks on a tract that is highlighted by plotly already, clear that highlighted polygon from the hover_polygon layer
      
      proxy %>% clearGroup("hover_polygon")
      
    } else {
      #when the user clicks a tract that is already in selected$groups, remove that tract from selected$groups and remove it from the second layer
      
      selected$groups <- setdiff(selected$groups, str_remove(input$map_shape_click$id, "^Tract "))
      
      proxy %>% clearGroup(input$map_shape_click$group)
      
    }
  }, ignoreInit = TRUE)
  
  #when the user highlights a tract in the plotly graph, add a hover_polygon layer to the leaflet map with only that tract
  observeEvent(plotly_hover_event_reactive(), { 
    
    leaflet_pal <- colorFactor(palette_reactive(), selected_tracts_geo_reactive()$GEOID)
    
    proxy %>% 
      clearGroup("hover_polygon") %>% 
      addPolygons(data = ac_tracts_reactive() %>% 
                    semi_join(plotly_hover_event_reactive(), by = c("GEOID" = "customdata")),
                  fillColor = ~leaflet_pal(GEOID),
                  fillOpacity = 1,
                  color = "black",
                  weight = 3,
                  label = ~GEOID,
                  group = "hover_polygon")
    
  })
  
  #turn user-selected tracts into a df, join to sf df and data source df
  geoid_table_reactive <- reactive({
    
    req(length(selected$groups) > 0)
    
    selected$groups %>% 
      enframe(value = "GEOID") %>% 
      select(-name) %>% 
      left_join(st_drop_geometry(ac_tracts_reactive()), by = "GEOID") %>% 
      select(NAME, GEOID) %>% 
      left_join(data_source_reactive()) %>% 
      filter(between(year, input$year_slider[1], input$year_slider[2]))
    
  })
  
  #create table to show data about user-selected tracts
  output$summary_table <- DT::renderDataTable({
    
    req(geoid_table_reactive())
    
    #if the data has the category column, filter the categories based on user selection from input$categories
    if ("category" %in% names(data_source_reactive())){
      
      req(input$categories)
      
      x <- geoid_table_reactive() %>% 
        filter(category %in% input$categories)
      
    }
    
    else {
      
      x <- geoid_table_reactive()
      
    }
    
    #extract and clean variable name
    var_name <- x %>% 
      distinct(variable) %>% 
      pull()
    
    var_name_proper <- var_name %>% 
      str_replace_all("_", " ") %>% 
      str_to_title()
    
    #drop NAME column
    table_df <- x %>% 
      select(-c(NAME))
    
    #clean column names
    table_df_names <- names(table_df) %>% 
      str_replace("moe", "Margin of Error") %>%
      str_replace("estimate", var_name) %>%
      str_replace("tract_year", "Census Year") %>% 
      str_replace("year", "Year") %>%
      str_replace("category", "Category") %>% 
      str_replace(var_name, var_name_proper)
    
    #update column names
    names(table_df) <- table_df_names
    
    #push to DT output
    table_df %>% 
      select(-variable) %>% 
      DT::datatable(options = list(autoWidth = TRUE,
                                   searching = FALSE,
                                   lengthChange = FALSE,
                                   pageLength = 5),
                    filter = "none")
    
  })
  
  
  #create graph that is dynamically generated based on user-selected data source and tracts
  output$plotly_graph <- renderPlotly({
    
    req(geoid_table_reactive())
    
    #determine if teh data is in percent units
    is_percent <- geoid_table_reactive() %>% 
      distinct(unit) %>% 
      pull() == "percent"
    
    #extract column names
    column_names <- names(geoid_table_reactive())
    
    #if the data has a margin of error and is in percent units, bound the moe between 0 and 1
    if ("moe" %in% column_names & is_percent) {
      
      x <- geoid_table_reactive() %>% 
        mutate(lower_bound = estimate - moe,
               upper_bound = estimate + moe,
               lower_bound = case_when(lower_bound < 0 ~ 0,
                                       lower_bound >= 0 ~ lower_bound),
               upper_bound = case_when(upper_bound > 1 ~ 1,
                                       upper_bound <= 1 ~ upper_bound))
      
      #if it has margin of error, bound the lower margin at 0
    } else if ("moe" %in% column_names & !is_percent) {
      
      x <- geoid_table_reactive() %>% 
        mutate(lower_bound = estimate - moe,
               upper_bound = estimate + moe,
               lower_bound = case_when(lower_bound < 0 ~ 0,
                                       lower_bound >= 0 ~ lower_bound))
    } else {
      
      x <- geoid_table_reactive()
      
    }
    
    #filter on category based on user-selected input$categories
    if ("category" %in% names(data_source_reactive())){
      
      req(input$categories)
      
      x <- x %>% 
        filter(category %in% input$categories)
      
    }
    
    else {
      
      x
      
    }
    
    #make the graph. pass custom palette to make_graph function
    x %>% 
      make_graph(custom_palette = palette_reactive()) %>% 
      ggplotly() %>% 
      highlight(on = "plotly_hover", off = "plotly_doubleclick") %>% 
      layout(showlegend = FALSE)
    
  })
  
  #capture which tract the user is hovering on in the plotly graph
  plotly_hover_event_reactive <- reactive({
    
    req(geoid_table_reactive())
    
    event_data("plotly_hover")
  })
  
  #not sure that this is being used anymore
  output$hover <- renderPrint({
    
    req(plotly_hover_event_reactive())
    
    plotly_hover_event_reactive()
    
  })
  
  #if the data source has the category column, show UI element that user can use to filter categories
  output$category_filter <- renderUI({
    
    if("category" %in% names(data_source_reactive())){
      
      selectizeInput(inputId = "categories",
                     label = "Select categories",
                     choices = data_source_reactive() %>% 
                       distinct(category) %>% 
                       pull(),
                     selected = data_source_reactive() %>% 
                       distinct(category) %>% 
                       slice_head(n = 3) %>% 
                       pull(),
                     multiple = TRUE)
    } else NULL
    
  })
  
}