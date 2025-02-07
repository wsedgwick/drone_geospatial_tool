

# Define UI
ui <- fluidPage(
  titlePanel("Drone Image and Orthomosaic Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      shinyDirButton("folder_select", "Select Image Folder", "Choose a folder with drone images"),
      textOutput("selected_folder"),
      shinyFilesButton("ortho_select", "Select Orthomosaic (GeoTIFF)", "Choose a GeoTIFF file", multiple = FALSE),
      actionButton("process", "Process Images"),
      br(), br(),
      DTOutput("image_table"),
      sliderInput("marker_size", "Marker Size:", min = 1, max = 10, value = 3, step = 1),
      selectInput("marker_color", "GPS Point Color:", 
                  choices = c("Red" = "red", "Black" = "black", "Yellow" = "yellow", "White" = "white"),
                  selected = "red"),
      radioButtons("map_type", "Select Map Type:",
                   choices = c("Street Map" = "OSM", "Satellite" = "Esri"),
                   selected = "OSM"),
    ),
    
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)

