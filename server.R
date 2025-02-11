server <- function(input, output, session) {
  
  valid_colors <- c("red", "white", "black", "blue")
  
  images_data_list <- reactiveVal(list())  # Store multiple datasets
  selected_folders <- reactiveVal(character())  # Store selected folder paths
  folder_colors <- reactiveVal(list())  # Store colors for each folder
  
  observe({
    shinyDirChoose(input, "folder_select", roots = volumes, session = session)
  })
  
  selected_folder_path <- reactiveVal(NULL)  # Store preview folder path
  preview_images <- reactiveVal(NULL)  # Store preview image filenames
  
  observe({
    shinyDirChoose(input, "folder_select", roots = volumes, session = session)
  })
  
  observe({
    req(input$folder_select)
    
    # Get the selected folder path
    folder_path <- parseDirPath(volumes, input$folder_select)
    selected_folder_path(folder_path)
    
    # Preview image files in the selected folder
    image_files <- list.files(folder_path, pattern = "\\.jpg$", full.names = FALSE, ignore.case = TRUE)
    preview_images(data.frame("Image Name" = image_files))
  })
  
  # Display the selected folder path before adding
  output$preview_folder_path <- renderText({
    req(selected_folder_path())
    paste("Selected Folder: ", selected_folder_path())
  })
  
  # Show the list of images before adding
  output$preview_image_table <- renderDT({
    req(preview_images())
    datatable(preview_images(), options = list(pageLength = 5), class = "compact")
  })
  
  observeEvent(input$add_folder, {
    req(input$folder_select)
    
    folder_path <- parseDirPath(volumes, input$folder_select)
    current_folders <- selected_folders()
    used_colors <- unname(unlist(folder_colors()))
    
    if (!(folder_path %in% current_folders)) {
      selected_folders(c(current_folders, folder_path))
      
      available_colors <- setdiff(valid_colors, used_colors)
      if (length(available_colors) == 0) {
        available_colors <- valid_colors  # Reset if all are used
      }
      new_color <- sample(available_colors, 1)
      
      folder_colors(c(folder_colors(), setNames(list(new_color), folder_path)))
      
      updateSelectInput(session, "selected_folder", choices = selected_folders(), selected = folder_path)
    }
  })
  
  observeEvent(selected_folders(), {
    updateSelectInput(session, "selected_folder", choices = selected_folders())
  })
  
  observeEvent(input$map_type, {
    leafletProxy("map") %>%
      clearTiles() %>%
      addProviderTiles(if (input$map_type == "Esri") providers$Esri.WorldImagery else providers$OpenStreetMap)
  })
  
  observe({
    req(selected_folders())
    
    all_coords <- NULL
    all_markers <- list()
    all_paths <- list()
    
    for (folder in selected_folders()) {
      folder_data <- images_data_list()[[folder]]
      folder_color <- folder_colors()[[folder]]
      
      if (!is.null(folder_data)) {
        coords <- st_coordinates(folder_data)
        
        all_coords <- rbind(all_coords, coords)
        
        all_markers[[folder]] <- leafletProxy("map") %>%
          addCircleMarkers(
            lng = coords[, 1],
            lat = coords[, 2],
            popup = paste0(
              "<b>Image:</b> ", folder_data$FileName, "<br>",
              "<b>Date:</b> ", folder_data$DateTimeOriginal, "<br>",
              {
                max_width <- 300  # Set max width
                image_width <- folder_data$ImageWidth
                image_height <- folder_data$ImageHeight
                
                aspect_ratio <- ifelse(!is.na(image_width) & !is.na(image_height) & image_height > 0, 
                                       image_width / image_height, 1.5)  # Default ratio fallback
                
                new_width <- min(image_width, max_width)
                new_height <- round(new_width / aspect_ratio)
                
                paste0("<img src='images/", folder_data$SourceFile, 
                       "' width='", new_width, "' height='", new_height, "'>")
              }
            ),
            radius = input$marker_size,
            stroke = FALSE,
            color = folder_color,
            fillColor = folder_color,
            fillOpacity = 1
          )
        
        if (input$show_path) {
          all_paths[[folder]] <- leafletProxy("map") %>%
            addPolylines(
              lng = coords[,1], lat = coords[,2], 
              color = folder_color, weight = 2, opacity = 0.8
            )
        }
      }
    }
    
    if (!is.null(all_coords)) {
      leafletProxy("map") %>%
        fitBounds(
          min(all_coords[,1], na.rm = TRUE), min(all_coords[,2], na.rm = TRUE),
          max(all_coords[,1], na.rm = TRUE), max(all_coords[,2], na.rm = TRUE)
        )
    }
  })
  
  observeEvent(input$add_folder, {
    req(input$folder_select)
    
    folder_path <- parseDirPath(volumes, input$folder_select)
    
    show("loading")  
    
    image_files <- list.files(folder_path, pattern = "\\.jpg$", full.names = TRUE, ignore.case = TRUE)
    addResourcePath("images", folder_path)
    
    withProgress(message = "Extracting image metadata...", value = 0, session = session, {
      n <- length(image_files)
      
      exif_data_list <- future_lapply(seq_along(image_files), function(i) {
        exifr::read_exif(image_files[i], tags = c("FileName", "GPSLongitude", "GPSLatitude", "GPSAltitude", "DateTimeOriginal"))
      })
      
      for (i in seq_along(exif_data_list)) {
        incProgress(1/n, session = session)
        Sys.sleep(0.01)  
      }
      
      exif_data <- dplyr::bind_rows(exif_data_list)
    })
    
    hide("loading")  
    
    exif_data <- dplyr::filter(exif_data, !is.na(GPSLongitude), !is.na(GPSLatitude))
    exif_data$DateTimeOriginal <- as.POSIXct(exif_data$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = "UTC")
    exif_data <- exif_data[order(exif_data$DateTimeOriginal), ]
    
    exif_data$SourceFile <- basename(exif_data$SourceFile)  
    exif_data <- sf::st_as_sf(exif_data, coords = c("GPSLongitude", "GPSLatitude"), crs = 4326)
    
    updated_data_list <- images_data_list()
    updated_data_list[[folder_path]] <- exif_data
    images_data_list(updated_data_list)
  })
  
  output$folder_legend <- renderUI({
    req(selected_folders())
    folder_colors_list <- folder_colors()
    
    legend_ui <- lapply(selected_folders(), function(folder) {
      color <- folder_colors_list[[folder]]
      selectInput(
        inputId = paste0("color_", gsub("[^a-zA-Z0-9]", "_", folder)), 
        label = basename(folder),
        choices = valid_colors, 
        selected = color
      )
    })
    
    do.call(tagList, legend_ui)
  })
  
  observe({
    req(selected_folders())
    folder_colors_list <- folder_colors()
    
    for (folder in selected_folders()) {
      color_input_id <- paste0("color_", gsub("[^a-zA-Z0-9]", "_", folder))
      new_color <- input[[color_input_id]]
      
      if (!is.null(new_color) && new_color %in% valid_colors) {
        folder_colors_list[[folder]] <- new_color
      }
    }
    
    folder_colors(folder_colors_list)
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(maxZoom = 22)) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
}
