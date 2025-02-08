ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Multi-Folder Drone Image Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      shinyDirButton("folder_select", "Select Image Folder", "Choose a folder with drone images"),
      br(), br(),
      textOutput("preview_folder_path"),
      DTOutput("preview_image_table"),
      actionButton("add_folder", "Add Selected Folder"),
      br(), br(),
      checkboxInput("show_path", "Show Drone Paths", value = FALSE),
      br(), br(),
      
      # Folder legend
      h4("Folders and Colors:"),
      uiOutput("folder_legend"),
      
      sliderInput("marker_size", "Marker Size:", min = 1, max = 10, value = 3, step = 1),
      
      # Select map type
      radioButtons("map_type", "Select Map Type:",
                   choices = c("Street Map" = "OSM", "Satellite" = "Esri"),
                   selected = "OSM")
    ),
    
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)