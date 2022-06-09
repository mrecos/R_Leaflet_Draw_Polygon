library(shinydashboard)
library(dashboardthemes)
library(leaflet)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(sf)
library(leaflet.extras)
library(DT)
library(rhandsontable)

# Define UI for application that draws a histogram
ui <- 
  dashboardPage(
    dashboardHeader(
      title = "Draw Polygons App"
    ),
    dashboardSidebar(
      textInput("file_name_input", "Enter file name",
                placeholder = "Default"),
      radioGroupButtons(
        inputId = "filetype",
        label = "File Type",
        choices = c("geojson","CSV"),
        selected = "geojson",
        checkIcon = list(
          yes = icon("ok", 
                     lib = "glyphicon")),
        status = "info"
      ),
      # Save Button
      downloadButton("downloadData", "Download",
                     style = "color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 12px; ")
      
    ),
    dashboardBody(
      useShinyjs(),
      shinyDashboardThemes(
        theme = "grey_light"
      ),
      fluidRow(
        column(width = 12,
               box(width = NULL, solidHeader = TRUE,
                   leafletOutput("map", height = 500)
               )
        )
      ),
      fluidRow(
        column(width = 12,
               box(width = NULL, solidHeader = TRUE, height = 500,
                   tableOutput("tbl")
                   # DTOutput("table1")
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
  
  # reactive container for table values
  # kept separate from coords b/c DT does not to sf objects
  # part of the design to allow edits/updates to table
  data_table <- reactiveValues(
    # DTtable = NULL
    DTtable = NULL
  )
  
  # output$table1 <- renderDT({
  #   req(data_table$DTtable)
  #   cols <- setdiff(colnames(data_table$DTtable),"_leaflet_id")
  #   datatable(data_table$DTtable[,cols], editable = TRUE)
  # })
  
  # drawn map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron",group = "CartoDB.Positron") %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery",group = "Esri.WorldImagery") %>%
      addLayersControl(
        baseGroups = c("CartoDB.Positron", "OpenStreetMap","Esri.WorldImagery"),
        position = "topright") %>% 
      setView(lng = -98, lat = 39, zoom = 4) %>% 
      addDrawToolbar(circleOptions = FALSE,
                     
                     polylineOptions = FALSE,
                     markerOptions = FALSE,
                     circleMarkerOptions = FALSE)
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
    # DT stuff
    # step_table <- st_drop_geometry(polygons$shapes)
    # step_table$LABEL <- NA
    # data_table$DTtable <- rbind(data_table$DTtable,
    #                             step_table)
    
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
