
## install required packages ---------------------------------------------- ----

req_packages = c("shiny", "leaflet", "sf")
for(pp in req_packages){
  if(pp %in% installed.packages())
    next
  install.packages(pp)
}

## setup ------------------------------------------------------------------ ----

library(shiny)
library(leaflet)
library(sf)

# working directory
working_directory = "E:/Downloads/area selection app/"
setwd(working_directory)

# Load shapefile (modify the path accordingly)
shapefile_path <- file.path(working_directory, "regional council shape file", "regional-council-2023-generalised.shp")
shape_data <- st_read(shapefile_path)
shape_data = st_transform(shape_data, crs = 4326)

shape_data$id = seq_len(nrow(shape_data))

## user interface --------------------------------------------------------- ----

# Define UI
ui <- fluidPage(
  leafletOutput("map"),
  verbatimTextOutput("selected_polygons")
)

## server ----------------------------------------------------------------- ----

# Define Server
server <- function(input, output, session) {
  
  # Reactive value to store selected polygon IDs
  selected_polygons <- reactiveVal(c())
  
  output$map <- renderLeaflet({
    leaflet(shape_data) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~id,
        color = "blue", 
        fillOpacity = 0.5, 
        highlight = highlightOptions(weight = 3, color = "red", bringToFront = TRUE)
      )
  })
  
  # Observe clicks to toggle selection
  observeEvent(input$map_shape_click, {
    clicked_id <- as.numeric(input$map_shape_click$id)

    if (clicked_id %in% selected_polygons()) {
      # Remove the polygon if already selected
      selected_polygons(setdiff(selected_polygons(), clicked_id))
    } else {
      # Add the polygon if not selected
      selected_polygons(c(selected_polygons(), clicked_id))
    }

    # Update map to reflect selection state
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(
        data = shape_data,
        layerId = ~id,
        color = ifelse(shape_data$id %in% selected_polygons(), "red", "blue"),
        fillOpacity = ifelse(shape_data$id %in% selected_polygons(), 0.8, 0.5),
        highlight = highlightOptions(weight = 3, color = "red", bringToFront = TRUE)
      )
  })
  
  # Display selected polygons
  output$selected_polygons <- renderPrint({
    shape_data[selected_polygons(), ]  # Show selected polygon data
  })
}

## execute app ------------------------------------------------------------ ----

# Run the application
shinyApp(ui, server)
