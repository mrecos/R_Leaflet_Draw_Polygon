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
                   # tableOutput("tbl")
                   DT::DTOutput("table1")
                   # div(DT::DTOutput("table1"),
                   #     style = "font-size: 75%; width: 75%")
               )
        )
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  # reactive container for the drawn polygon shapes
  polygons <- reactiveValues(
    shapes = NULL,
    joined = NULL
  )
  
  # reactive container for table values
  # kept separate from coords b/c DT does not to sf objects
  # part of the design to allow edits/updates to table
  data_table <- reactiveValues(
    DTtable = NULL
  )
  
  output$table1 <- renderDT({
    req(data_table$DTtable)
    cols <- setdiff(colnames(data_table$DTtable),"_leaflet_id")
    datatable(data_table$DTtable[,cols], editable = TRUE)
  })
  
  observeEvent(input$table1_cell_edit, {
    row  <- input$table1_cell_edit$row
    clmn <- input$table1_cell_edit$col
    data_table$DTtable[row, clmn+1] <- input$table1_cell_edit$value
    print(data_table$DTtable)
  })
  
  # # testing map and table interaction
  # observeEvent(input$table1_rows_selected, {
  #   print(input$table1_rows_selected)
  # })
  # 
  # observeEvent(input$map_shape_click, {
  #   
  #   #capture the info of the clicked polygon
  #   map_click <- input$map_shape_click
  #   print(paste0("click:",map_click))
  #   # #subset your table with the id of the clicked polygon 
  #   # selected <- mydata[mydata$myID == click$id,]
  #   # 
  #   # #if click id isn't null render the table
  #   # if(!is.null(click$id)){
  #   #   output$mytable = DT::renderDataTable({
  #   #     selected
  #   #   }) 
  #   # } 
  # })
  # ## end testing table / map interaction
  
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
    
    # output$tbl <- renderTable(st_drop_geometry(polygons$shapes))
    # DT - rbind new feature with LABEL column
    step_table <- st_drop_geometry(feature_sf)
    step_table$LABEL <- NA
    data_table$DTtable <- rbind(data_table$DTtable,
                                step_table)
    
  })
  
  # Downloadable geojson of polygons ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$file_name_input, ".", 
            tolower(input$filetype), sep = "")
    },
    content = function(file) {
      # join sf and edits from DT
      polygons$joined <- data_table$DTtable %>% 
        left_join(select(polygons$shapes, "_leaflet_id", geometry), 
                  by = "_leaflet_id") %>% 
        st_sf() %>% 
        st_transform(crs = "EPSG:4326")
      
      if(input$filetype == "CSV"){
        out_WKT <- st_as_text(polygons$joined$geometry)
        out_polys <- st_drop_geometry(polygons$joined)
        out_polys$geometry <- out_WKT
      } else if(input$filetype == "geojson"){
        out_polys <- polygons$joined
      }
      write_sf(out_polys, file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
