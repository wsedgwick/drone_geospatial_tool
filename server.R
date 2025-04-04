server <- function(input, output, session) {
  # Directory chooser for folder selection
  shinyDirChoose(input, "folder_select", roots = volumes, session = session)
  
  # Set of available marker colors
  valid_colors <- c("red", "white", "black", "blue")
  
  # Reactive containers to store image metadata, selected folders, and folder colors
  images_data_list <- reactiveVal(list())
  selected_folders <- reactiveVal(character())
  folder_colors <- reactiveVal(list())
  
  # STEP 1: Add folder to list when selected
  observeEvent(input$folder_select, {
    folder_path <- tryCatch(parseDirPath(volumes, input$folder_select), error = function(e) NULL)
    if (is.null(folder_path) || length(folder_path) == 0 || is.na(folder_path) || folder_path == "") return()
    
    if (isTRUE(!(folder_path %in% selected_folders()))) {
      current_folders <- selected_folders()
      used_colors <- unname(unlist(folder_colors()))
      selected_folders(c(current_folders, folder_path))
      
      available_colors <- setdiff(valid_colors, used_colors)
      if (length(available_colors) == 0) available_colors <- valid_colors
      new_color <- sample(available_colors, 1)
      
      folder_colors(c(folder_colors(), setNames(list(new_color), folder_path)))
    }
  })
  
  # STEP 2: Extract metadata for new folders (with caching)
  observeEvent(selected_folders(), {
    new_folders <- setdiff(selected_folders(), names(images_data_list()))
    
    for (folder_path in new_folders) {
      show("loading")
      image_files <- list.files(folder_path, pattern = "\\.jpg$", full.names = TRUE, ignore.case = TRUE)
      
      # Register a virtual image path for leaflet popups
      virtual_path <- paste0("images_", digest::digest(folder_path))
      addResourcePath(virtual_path, folder_path)
      
      metadata_path <- file.path(folder_path, "metadata.rds")
      
      if (file.exists(metadata_path)) {
        exif_data <- readRDS(metadata_path)
      } else {
        withProgress(message = "Extracting image metadata...", value = 0, session = session, {
          chunk_size <- 50
          chunks <- split(image_files, ceiling(seq_along(image_files) / chunk_size))
          total_chunks <- length(chunks)
          
          exif_data_list <- list()
          for (i in seq_along(chunks)) {
            chunk <- chunks[[i]]
            chunk_exif <- future_lapply(chunk, function(img) {
              exifr::read_exif(img, tags = c("FileName", "GPSLongitude", "GPSLatitude", "GPSAltitude", "DateTimeOriginal"))
            })
            exif_data_list <- c(exif_data_list, chunk_exif)
            incProgress(1 / total_chunks, detail = paste0("Loaded ", i * chunk_size, " / ", length(image_files), " images"))
          }
          exif_data <- dplyr::bind_rows(exif_data_list)
          saveRDS(exif_data, metadata_path)
        })
      }
      
      hide("loading")
      
      # Clean metadata and convert to sf object
      exif_data <- dplyr::filter(exif_data, !is.na(GPSLongitude), !is.na(GPSLatitude))
      exif_data$DateTimeOriginal <- as.POSIXct(exif_data$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = "UTC")
      exif_data <- exif_data[order(exif_data$DateTimeOriginal), ]
      exif_data$SourceFile <- basename(exif_data$SourceFile)
      exif_data$virtual_path <- virtual_path
      exif_data <- sf::st_as_sf(exif_data, coords = c("GPSLongitude", "GPSLatitude"), crs = 4326)
      
      updated_data_list <- images_data_list()
      updated_data_list[[folder_path]] <- exif_data
      images_data_list(updated_data_list)
    }
  })
  
  # UI: Determine which folders are checked
  checked_folders <- reactive({
    folders <- selected_folders()
    checks <- sapply(folders, function(f) {
      val <- input[[paste0("chk_", gsub("[^a-zA-Z0-9]", "_", f))]]
      if (is.null(val)) NA else val
    })
    checks <- as.logical(checks)
    folders[is.na(checks) | checks]
  })
  
  # UI: Folder controls for checkbox, color picker, and delete button
  output$folder_controls <- renderUI({
    req(selected_folders())
    folders <- selected_folders()
    checked <- checked_folders()
    colors <- folder_colors()
    
    controls <- lapply(folders, function(folder) {
      fid <- gsub("[^a-zA-Z0-9]", "_", folder)
      fluidRow(
        column(1, checkboxInput(paste0("chk_", fid), NULL, value = folder %in% checked)),
        column(6, tags$label(basename(folder))),
        column(3, selectInput(paste0("color_", fid), NULL, choices = valid_colors, selected = colors[[folder]], width = "100%")),
        column(2, actionButton(paste0("rm_", fid), "✖", class = "btn-xs btn-danger"))
      )
    })
    tagList(controls)
  })
  
  # UI: Render metadata tables per folder
  output$folder_tables <- renderUI({
    req(checked_folders())
    data_list <- images_data_list()
    lapply(checked_folders(), function(folder) {
      if (!is.null(data_list[[folder]])) {
        tagList(h5(basename(folder)), DTOutput(paste0("table_", digest::digest(folder))))
      }
    }) %>% tagList()
  })
  
  # Table content render logic
  observe({
    req(checked_folders())
    data_list <- images_data_list()
    for (folder in checked_folders()) {
      local({
        f <- folder
        output_id <- paste0("table_", digest::digest(f))
        output[[output_id]] <- renderDT({
          req(!is.null(data_list[[f]]))
          sf_data <- data_list[[f]]
          coords <- st_coordinates(sf_data)
          df <- st_drop_geometry(sf_data)
          df$Longitude <- round(coords[, 1], 7)
          df$Latitude <- round(coords[, 2], 7)
          df <- dplyr::select(df, FileName, DateTimeOriginal, Longitude, Latitude, GPSAltitude)
          colnames(df) <- c("Filename", "DateTime", "Longitude", "Latitude", "Altitude")
          df$DateTime <- format(df$DateTime, "%Y-%m-%d %H:%M:%S")
          df$Altitude <- paste0(round(as.numeric(df$Altitude), 1), " m")
          datatable(df, options = list(pageLength = 5, scrollX = TRUE), class = "compact nowrap")
        })
      })
    }
  })
  
  # Reactively track color changes from dropdowns
  observe({
    folders <- selected_folders()
    colors <- folder_colors()
    for (folder in folders) {
      local({
        f <- folder
        fid <- gsub("[^a-zA-Z0-9]", "_", f)
        input_id <- paste0("color_", fid)
        color_input <- input[[input_id]]
        if (!is.null(color_input) && color_input != colors[[f]]) {
          colors[[f]] <- color_input
          folder_colors(colors)
        }
      })
    }
  })
  
  # Remove folder and clear map group when 'X' button clicked
  observeEvent(selected_folders(), {
    folders <- selected_folders()
    for (folder in folders) {
      local({
        f <- folder
        fid <- paste0("rm_", gsub("[^a-zA-Z0-9]", "_", f))
        observeEvent(input[[fid]], {
          selected_folders(setdiff(selected_folders(), f))
          colors <- folder_colors(); colors[[f]] <- NULL; folder_colors(colors)
          dat <- images_data_list(); dat[[f]] <- NULL; images_data_list(dat)
          leafletProxy("map") %>% clearGroup(f)
        }, ignoreInit = TRUE, once = TRUE)
      })
    }
  })
  
  # Toggle between basemaps
  observeEvent(input$map_type, {
    leafletProxy("map") %>%
      clearTiles() %>%
      addProviderTiles(if (input$map_type == "Esri") providers$Esri.WorldImagery else providers$OpenStreetMap)
  })
  
  # Map rendering logic: markers, lines, zoom
  observe({
    req(selected_folders())
    req(folder_colors())
    
    proxy <- leafletProxy("map")
    for (folder in selected_folders()) {
      proxy <- proxy %>% clearGroup(folder)
    }
    
    all_coords <- NULL
    
    for (folder in selected_folders()) {
      folder_data <- images_data_list()[[folder]]
      folder_color <- folder_colors()[[folder]]
      if (!is.null(folder_data)) {
        coords <- sf::st_coordinates(folder_data)
        all_coords <- rbind(all_coords, coords)
        
        proxy <- proxy %>% addCircleMarkers(
          lng = coords[, 1], lat = coords[, 2],
          popup = paste0(
            "<b>Image:</b> ", folder_data$FileName, "<br>",
            "<b>Date:</b> ", folder_data$DateTimeOriginal, "<br>",
            paste0("<img src='", folder_data$virtual_path, "/", folder_data$SourceFile, "' width='300'>")
          ),
          radius = input$marker_size,
          stroke = FALSE,
          color = folder_color,
          fillColor = folder_color,
          fillOpacity = 1,
          group = folder
        )
        
        if (isTRUE(input$show_path)) {
          proxy <- proxy %>% addPolylines(
            lng = coords[, 1], lat = coords[, 2],
            color = folder_color, weight = 2, opacity = 0.8,
            group = folder
          )
        }
      }
    }
    
    if (!is.null(all_coords) && nrow(all_coords) > 0) {
      proxy %>% fitBounds(
        lng1 = min(all_coords[,1], na.rm = TRUE),
        lat1 = min(all_coords[,2], na.rm = TRUE),
        lng2 = max(all_coords[,1], na.rm = TRUE),
        lat2 = max(all_coords[,2], na.rm = TRUE)
      )
    }
  })
  
  # Initial map rendering
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(maxZoom = 22)) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
}
