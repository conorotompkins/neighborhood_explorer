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

ui <- fluidPage(
  
  fluidRow(
    
    column(width = 2,
           
           selectizeInput(inputId = "data_source",
                          label = "Choose topic",
                          choices = c("median_income", "housing", "commute_modes"))
           
    ),
    
    column(width = 3,
           
           sliderInput(inputId = "year_slider",
                       label = "Year",
                       value = c(2010, 2019),
                       min = 2010,
                       max = 2019,
                       sep = "")
    ),
    
    column(width = 3,
           
           uiOutput("category_filter")
           
    )
  ),
  
  column(width = 10,
         
         fluidRow(
           
           leafletOutput("map")
           
         ),
         
         fluidRow(
           
           tabsetPanel(
             
             tabPanel(title = "Graph",
                      plotlyOutput("bar_chart")),
             
             tabPanel(title = "Table",
                      DT::dataTableOutput("geoid_table"))
             
           )
           
         )
  )
  
)

server <- function(input, output, session){
  
  #modal at start
  observeEvent(1, {
    
    showModal(
      
      modalDialog(
        
        title = "Tip",
        "Click on census tracts to start!"
        
      )
    )
    
  })
  
  data_source_reactive <- reactive({
    
    get_data(input$data_source)
    
  })
  
  observeEvent(data_source_reactive(), {
    
    print(data_source_reactive())
    print(names(data_source_reactive()))
    
    year_min <- min(data_source_reactive()$year)
    year_max <- max(data_source_reactive()$year)
    
    year_step <- ifelse(year_min == 1940, 10, 1)
    
    updateSliderInput(inputId = "year_slider",
                      value = c(year_min, year_max),
                      min = year_min,
                      max = year_max,
                      step = year_step)
    
  })
  
  tract_year_reactive <- reactive({
    
    isolate(data_source_reactive()) %>% 
      distinct(tract_year) %>% 
      pull()
    
  })
  
  ac_tracts_reactive <- reactive({
    
    ac_geo %>% 
      filter(tract_year == tract_year_reactive())
    
  })
  
  #create empty vector to hold all click ids
  selected <- reactiveValues(groups = vector())
  
  #reset selected tracts when tract_year changes
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
                  layerId = ~NAME,
                  group = "base_map",
                  label = ~NAME)
  }) #END RENDER LEAFLET
  
  #define leaflet proxy for second regional level map
  proxy <- leafletProxy("map")
  
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
  
  observeEvent(input$map_shape_click, {
    
    if(input$map_shape_click$group == "base_map"){
      #when the user clicks a polygon on the basemap, add that polygon to selected$groups and display the new layer
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
                    layerId = ~GEOID,
                    group = ~GEOID,
                    label = ~GEOID)
    } else if(input$map_shape_click$group == "hover_polygon") {
      #when the user clicks on a tract that is highlighted by plotly already, clear that highlight polygon from the map
      
      proxy %>% clearGroup("hover_polygon")
      
    } else {
      #when the user clicks a tract that is already in selected$groups, remove that tract from selected$groups and remove it from the second layer
      selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
      
      proxy %>% clearGroup(input$map_shape_click$group)
      
    }
  }, ignoreInit = TRUE)
  
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
  
  output$geoid_table <- DT::renderDataTable({
    
    req(geoid_table_reactive())
    
    if ("category" %in% names(data_source_reactive())){
      
      req(input$categories)
      
      x <- geoid_table_reactive() %>% 
        filter(category %in% input$categories)
      
    }
    
    else {
      
      x
      
    }
    
    var_name <- x %>% 
      distinct(variable) %>% 
      pull()
    
    var_name_proper <- var_name %>% 
      str_replace_all("_", " ") %>% 
      str_to_title()
    
    table_df <- x %>% 
      select(-c(NAME))
    
    table_df_names <- names(table_df) %>% 
      str_replace("moe", "Margin of Error") %>%
      str_replace("estimate", var_name) %>%
      str_replace("tract_year", "Census Year") %>% 
      str_replace("year", "Year") %>%
      str_replace("category", "Category") %>% 
      str_replace(var_name, var_name_proper)
    
    names(table_df) <- table_df_names
    
    table_df %>% 
      select(-variable) %>% 
      DT::datatable(options = list(autoWidth = TRUE,
                                   searching = FALSE,
                                   lengthChange = FALSE,
                                   pageLength = 5),
                    filter = "none")
    
  })
  
  
  
  output$bar_chart <- renderPlotly({
    
    req(geoid_table_reactive())
    
    if ("moe" %in% names(geoid_table_reactive())) {
      
      x <- geoid_table_reactive() %>% 
        mutate(lower_bound = estimate - moe,
               upper_bound = estimate + moe,
               lower_bound = case_when(lower_bound < 0 ~ 0,
                                       lower_bound >= 0 ~ lower_bound))
    } else {
      
      x <- geoid_table_reactive()
      
    }
    
    if ("category" %in% names(data_source_reactive())){
      
      req(input$categories)
      
      x <- x %>% 
        filter(category %in% input$categories)
      
    }
    
    else {
      
      x
      
    }
    
    x %>% 
      make_graph(custom_palette = palette_reactive()) %>% 
      ggplotly() %>% 
      highlight(on = "plotly_hover", off = "plotly_doubleclick")
    
  })
  
  plotly_hover_event_reactive <- reactive({
    
    req(geoid_table_reactive())
    
    event_data("plotly_hover")
  })
  
  output$hover <- renderPrint({
    
    req(plotly_hover_event_reactive())
    
    plotly_hover_event_reactive()
    
  })
  
  output$category_filter <- renderUI({
    
    print(data_source_reactive())
    
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

# Run the application 
shinyApp(ui = ui, server = server)
