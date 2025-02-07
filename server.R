server <- function(input, output, session) {
  
  images_data <- reactiveVal(NULL)
  ortho_raster <- reactiveVal(NULL)
  
  observe({
    shinyDirChoose(input, "folder_select", roots = volumes, session = session)
    shinyFileChoose(input, "ortho_select", roots = volumes, session = session, filetypes = c("tif", "tiff"))
  })
  
  # PROCESS images when button is clicked
  observeEvent(input$process, {
    req(input$folder_select)
    
    folder_path <- parseDirPath(volumes, input$folder_select)
    image_files <- list.files(folder_path, pattern = "\\.jpg$", full.names = TRUE, ignore.case = TRUE)
    
    addResourcePath("images", folder_path)
    
    # Extract EXIF metadata
    exif_data <- exifr::read_exif(image_files, tags = c("FileName", "GPSLongitude", "GPSLatitude", "GPSAltitude",
                                                        "Yaw", "Pitch", "Roll", "ImageWidth", "ImageHeight", "DateTimeOriginal"))
    
    # Remove images that do not have GPS data
    exif_data <- exif_data[!is.na(exif_data$GPSLongitude) & !is.na(exif_data$GPSLatitude), ]
    
    # Convert to spatial points
    exif_data$SourceFile <- basename(exif_data$SourceFile)  
    exif_data <- st_as_sf(exif_data, coords = c("GPSLongitude", "GPSLatitude"), crs = 4326)
    images_data(exif_data)
  })
  
  # RENDER Leaflet Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
  
  # DISPLAY image data
  observe({
    req(images_data())  
    
    exif_sf <- images_data()  
    
    coords <- st_coordinates(exif_sf)
    
    center_lng <- mean(coords[, 1], na.rm = TRUE)
    center_lat <- mean(coords[, 2], na.rm = TRUE)
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = coords[, 1],
        lat = coords[, 2],
        popup = paste0(
          "<b>Image:</b> ", exif_sf$FileName, "<br>",
          "<b>Date:</b> ", exif_sf$DateTimeOriginal, "<br>",
          "<b>Altitude:</b> ", ifelse(!is.na(exif_sf$GPSAltitude), paste0(exif_sf$GPSAltitude, " m"), "N/A"), "<br>",
          "<b>Roll:</b> ", ifelse(!is.na(exif_sf$Roll), exif_sf$Roll, "N/A"), "°<br>",
          "<b>Pitch:</b> ", ifelse(!is.na(exif_sf$Pitch), exif_sf$Pitch, "N/A"), "°<br>",
          "<b>Yaw:</b> ", ifelse(!is.na(exif_sf$Yaw), exif_sf$Yaw, "N/A"), "°<br>",
          "<img src='images/", exif_sf$SourceFile, "' width='300' height='200'>"
          # "<img src='images/", exif_sf$SourceFile, "' style='max-width: 100%; height: auto; display: block; margin-top: 5px;'>
        # </div>"
        ),
        radius = input$marker_size,
        stroke = FALSE,
        color = input$marker_color,
        fillColor = input$marker_color,
        fillOpacity = 1
      ) %>% setView(lng = center_lng, lat = center_lat, zoom = 14)
  })
  
  observe({
    req(input$map_type)
    
    leafletProxy("map") %>%
      clearTiles() %>%
      addProviderTiles(
        if (input$map_type == "Esri") providers$Esri.WorldImagery else providers$OpenStreetMap
      )
  })
  
  # Display EXIF metadata table
  output$image_table <- renderDT({
    req(images_data())
    datatable(as.data.frame(images_data())[, c("FileName", "DateTimeOriginal")], 
              selection = "single",  
              options = list(pageLength = 10))
  })
  
  output$selected_folder <- renderText({
    req(input$folder_select)
    folder_path <- parseDirPath(volumes, input$folder_select)
    paste("Selected Folder:", folder_path)
  })
  
  observeEvent(input$image_table_rows_selected, {
    req(images_data())
    
    selected_row <- input$image_table_rows_selected
    if (length(selected_row) == 0) return()
    
    selected_point <- images_data()[selected_row, ]
    coords <- st_coordinates(selected_point)
    
    # folder_path <- parseDirPath(volumes, input$folder_select)
    
    # Construct the absolute file path to the image
    # image_path <- normalizePath(file.path(folder_path, selected_point$FileName), winslash = "/")
    
    
    leafletProxy("map") %>%
      setView(lng = coords[1, 1], lat = coords[1, 2], zoom = 17) %>%  
      addPopups(coords[1, 1], coords[1, 2], paste0(
        "<b>Image:</b> ", selected_point$FileName, "<br>",
        "<b>Date:</b> ", selected_point$DateTimeOriginal, "<br>",
        "<a href='file:///", selected_point$FileName, "' target='_blank'>",
        "<img src='images/", selected_point$FileName, "' width='300' height='200'></a>"
      ))
  })
}
