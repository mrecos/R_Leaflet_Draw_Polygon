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
      textInput("file_name_input", "enter file name",
                placeholder = "Default"),
      # Save Button
      downloadButton("downloadData", "Download")
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
               )
        ),
        column(width = 3,
          box(width = NULL, solidHeader = TRUE, height = 500,
              tableOutput("tbl")
            )
        )
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # reactive container for the drawn polygon shapes
  polygons <- reactiveValues(
    shapes = NULL
  )
  
  # drawn map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98, lat = 39, zoom = 4) %>% 
      addDrawToolbar()
  })
  
  # proxy not in use at the moment
  # mymap_proxy <- leafletProxy("map")
  
  # Watch for new polygons to be drawn and add to sf dataframe
  observeEvent(input$map_draw_new_feature,{
    feature <- input$map_draw_new_feature
    
    feature_sf <- geojsonsf::geojson_sf(jsonify::to_json(feature, unbox = T))
    
    polygons$shapes <- rbind(polygons$shapes, feature_sf)
    print(polygons$shapes)
    
    output$tbl <- renderTable(st_drop_geometry(polygons$shapes))
    
  })
  
  # Downloadable geojson of polygons ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$file_name_input, ".geojson", sep = "")
    },
    content = function(file) {
      write_sf(polygons$shapes, file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
