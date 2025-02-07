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
    
    show("loading")  # Show loading message
    
    folder_path <- parseDirPath(volumes, input$folder_select)
    image_files <- list.files(folder_path, pattern = "\\.jpg$", full.names = TRUE, ignore.case = TRUE)
    
    addResourcePath("images", folder_path)
    
    # ✅ Use `withProgress()` to make progress visible
    withProgress(message = "Extracting image metadata...", value = 0, session = session, {
      
      n <- length(image_files)
      
      # ✅ Use `future_lapply()` for parallel processing, passing session manually
      exif_data_list <- future_lapply(seq_along(image_files), function(i) {
        # Extract metadata
        exifr::read_exif(image_files[i], tags = c("FileName", "GPSLongitude", "GPSLatitude", "GPSAltitude",
                                                  "Yaw", "Pitch", "Roll", "ImageWidth", "ImageHeight", "DateTimeOriginal"))
      })
      
      # 🔥 Correct way to update progress bar in UI
      for (i in seq_along(exif_data_list)) {
        incProgress(1/n, session = session)  # ✅ Pass `session` for UI updates
        Sys.sleep(0.01)  # ✅ Ensures UI refreshes (prevents skipping)
      }
      
      # ✅ Use `dplyr::bind_rows()` instead of `do.call(rbind, ...)`
      exif_data <- dplyr::bind_rows(exif_data_list)
    })
    
    hide("loading")  # Hide loading message when done
    
    # ✅ Remove invalid images early
    exif_data <- dplyr::filter(exif_data, !is.na(GPSLongitude), !is.na(GPSLatitude))
    
    # ✅ Convert DateTimeOriginal to proper format
    exif_data$DateTimeOriginal <- as.POSIXct(exif_data$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = "UTC")
    
    # ✅ Sort by date
    exif_data <- exif_data[order(exif_data$DateTimeOriginal), ]
    
    # ✅ Convert to spatial points
    exif_data$SourceFile <- basename(exif_data$SourceFile)  
    exif_data <- sf::st_as_sf(exif_data, coords = c("GPSLongitude", "GPSLatitude"), crs = 4326)
    
    images_data(exif_data)  # Store processed image data
  })
  
  
  
  # RENDER Leaflet Map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(maxZoom = 22)) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
  
  first_load <- reactiveVal(TRUE)
  # DISPLAY image data
  observe({
    req(images_data())  
    
    exif_sf <- images_data()  
    coords <- st_coordinates(exif_sf)
    
    current_view <- isolate(input$map_bounds)
    current_zoom <- isolate(input$map_zoom)
    
    # center_lng <- mean(coords[, 1], na.rm = TRUE)
    # center_lat <- mean(coords[, 2], na.rm = TRUE)
    if (is.null(current_view)) {
      center_lng <- mean(coords[, 1], na.rm = TRUE)
      center_lat <- mean(coords[, 2], na.rm = TRUE)
    } else {
      center_lng <- (current_view$east + current_view$west) / 2
      center_lat <- (current_view$north + current_view$south) / 2
    }
    
    map_proxy <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>% 
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
          
          # "<img src='images/", exif_sf$SourceFile, "' width='300' height='200'>"
        
          {
            max_width <- 300  # Set max width
            image_width <- exif_sf$ImageWidth
            image_height <- exif_sf$ImageHeight
            
            aspect_ratio <- ifelse(!is.na(image_width) & !is.na(image_height) & image_height > 0, 
                                   image_width / image_height, 1.5)  # Default ratio fallback
            
            new_width <- min(image_width, max_width)
            new_height <- round(new_width / aspect_ratio)
            
            paste0("<img src='images/", exif_sf$SourceFile, 
                   "' width='", new_width, "' height='", new_height, "'>")
          }
        
        ),
        radius = input$marker_size,
        stroke = FALSE,
        color = input$marker_color,
        fillColor = input$marker_color,
        fillOpacity = 1
      ) 
    
    if (first_load()) {
      center_lng <- mean(coords[, 1], na.rm = TRUE)
      center_lat <- mean(coords[, 2], na.rm = TRUE)
      
      map_proxy %>% setView(lng = center_lng, lat = center_lat, zoom = 14)
      
      first_load(FALSE)  # ✅ Disable `setView()` after first use
    }
    
    # setView(lng = center_lng, lat = center_lat, zoom = 14)
    if (input$show_path) {
      map_proxy %>% addPolylines(lng = coords[,1], lat = coords[,2], color = "blue", weight = 2, opacity = 0.8)
    }
    
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
  # output$image_table <- renderDT({
  #   req(images_data())
  #   datatable(as.data.frame(images_data())[, c("FileName", "DateTimeOriginal")], 
  #             selection = "single",  
  #             options = list(pageLength = 10))
  # })
  
  output$image_table <- renderDT({
    req(images_data())
    
    # Convert to a data frame
    df <- as.data.frame(images_data())
    
    # Ensure DateTimeOriginal is properly formatted
    df$DateTimeOriginal <- as.POSIXct(df$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = "UTC")
    
    # Create separate Date and Time columns
    df$Date <- format(df$DateTimeOriginal, "%Y-%m-%d")  # Extract Date
    df$Time <- format(df$DateTimeOriginal, "%H:%M:%S")  # Extract Time
    
    # Select and rename columns
    df <- df[, c("FileName", "Date", "Time")]  # Keep only these columns
    colnames(df) <- c("File Name", "Date", "Time")  # Rename columns
    
    datatable(df, 
              selection = "single",  
              options = list(pageLength = 10),
              class = "compact")
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
        {
          max_width <- 300  # Set max width
          image_width <- selected_point$ImageWidth
          image_height <- selected_point$ImageHeight
          
          aspect_ratio <- ifelse(!is.na(image_width) & !is.na(image_height) & image_height > 0, 
                                 image_width / image_height, 1.5)  # Default ratio fallback
          
          new_width <- min(image_width, max_width)
          new_height <- round(new_width / aspect_ratio)
          
          paste0("<img src='images/", selected_point$FileName,
                 "' width='", new_width, "' height='", new_height, "'>")
        }
        # "<img src='images/", selected_point$FileName, "' width='300' height='200'></a>"
      ))
  })
}
