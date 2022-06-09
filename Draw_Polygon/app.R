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
      downloadButton("downloadData", "Download"),
      radioGroupButtons(
        inputId = "filetype",
        label = "File Type",
        choices = c("geojson","CSV"),
        selected = "geojson"
      )
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
  print(input$filetype)
    feature_sf <- geojsonsf::geojson_sf(jsonify::to_json(feature, unbox = T))
    
    polygons$shapes <- rbind(polygons$shapes, feature_sf)
    print(polygons$shapes)
    
    output$tbl <- renderTable(st_drop_geometry(polygons$shapes))
    
  })
  
  # Downloadable geojson of polygons ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$file_name_input, ".", 
            tolower(input$filetype), sep = "")
    },
    content = function(file) {
      if(input$filetype == "CSV"){
        out_WKT <- st_as_text(polygons$shapes$geometry)
        out_polys <- st_drop_geometry(polygons$shapes)
        out_polys$geometry <- out_WKT
      } else if(input$filetype == "geojson"){
        out_polys <- polygons$shapes
      }
      write_sf(out_polys, file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
