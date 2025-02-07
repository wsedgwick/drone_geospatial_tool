

# Define UI
ui <- fluidPage(
  titlePanel("Drone Image and Orthomosaic Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      shinyDirButton("folder_select", "Select Image Folder", "Choose a folder with drone images"),
      br(), br(),
      textOutput("selected_folder"),
      shinyFilesButton("ortho_select", "Select Orthomosaic (GeoTIFF)", "Choose a GeoTIFF file", multiple = FALSE),
      actionButton("process", "Process Images"),
      br(), br(),
      DTOutput("image_table"),
      sliderInput("marker_size", "Marker Size:", min = 1, max = 10, value = 3, step = 1),
      selectInput("marker_color", "GPS Point Color:", 
                  choices = c("Red" = "red", "Black" = "black", "Yellow" = "yellow", "White" = "white"),
                  selected = "red"),
      
      # Wrap "Select Map Type" and "Show Drone Path" checkbox in a div for inline layout
      div(style = "display: flex; align-items: center; gap: 10px;",
          radioButtons("map_type", "Select Map Type:",
                       choices = c("Street Map" = "OSM", "Satellite" = "Esri"),
                       selected = "OSM"),
          checkboxInput("show_path", "Show Drone Path", value = TRUE)  # Checkbox for toggling polylines
      )
    ),
    
    
    
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)

