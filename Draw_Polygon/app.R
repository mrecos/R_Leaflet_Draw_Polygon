library(shinydashboard)
library(dashboardthemes)
library(leaflet)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(sf)
library(leaflet.extras)

# Define UI for application that draws a histogram
ui <- 
  dashboardPage(
    dashboardHeader(
      title = "Draw Polygons App"
    ),
    dashboardSidebar(
      
    ),
    dashboardBody(
      useShinyjs(),
      shinyDashboardThemes(
        theme = "grey_light"
      ),
      fluidRow(
        column(width = 9,
               box(width = NULL, solidHeader = TRUE,
                   leafletOutput("map", height = 500)
               ),
               box(width = NULL,
                   tableOutput("tbl")
               )
        )
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    cat("output$map","\n")
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98, lat = 39, zoom = 4) %>% 
      addDrawToolbar()
  })
  
  mymap_proxy <- leafletProxy("map")
  
  observeEvent(input$map_draw_new_feature,{
    feature <- input$map_draw_new_feature
    
    feature_sf <- geojsonsf::geojson_sf(jsonify::to_json(feature, unbox = T))
    print(feature_sf)
    
    output$tbl <- renderTable(st_drop_geometry(feature_sf))
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
