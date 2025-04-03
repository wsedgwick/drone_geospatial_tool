ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Multi-Folder Drone Image Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      shinyDirButton("folder_select", "Select Image Folder", "Choose a folder with drone images"),

      
      # Folder legend
      h4("Folders:"),
      uiOutput("folder_controls"),
      br(),
      h4("Folder Metadata:"),
      uiOutput("folder_tables"),
      checkboxInput("show_path", "Show Drone Paths", value = FALSE),
      
      sliderInput("marker_size", "Marker Size:", min = 1, max = 10, value = 3, step = 1),
      
      # Select map type
      radioButtons("map_type", "Select Map Type:",
                   choices = c("Street Map" = "OSM", "Satellite" = "Esri"),
                   selected = "Esri")
    ),
    
    mainPanel(
      leafletOutput("map", height = "600px"),
    )
  )
)