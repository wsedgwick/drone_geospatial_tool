server <- function(input, output, session) {
  
  valid_colors <- c("red", "white", "black", "blue")
  
  images_data_list <- reactiveVal(list())
  selected_folders <- reactiveVal(character())
  folder_colors <- reactiveVal(list())
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  observe({
    shinyDirChoose(input, "folder_select", roots = volumes, session = session)
  })
  
  selected_folder_path <- reactiveVal(NULL)
  preview_images <- reactiveVal(NULL)
  
  observe({
    req(input$folder_select)
    folder_path <- parseDirPath(volumes, input$folder_select)
    selected_folder_path(folder_path)
    
    image_files <- list.files(folder_path, pattern = "\\.jpg$", full.names = FALSE, ignore.case = TRUE)
    preview_images(data.frame("Image Name" = image_files))
  })
  
  observeEvent(input$add_folder, {
    req(input$folder_select)
    folder_path <- parseDirPath(volumes, input$folder_select)
    current_folders <- selected_folders()
    used_colors <- unname(unlist(folder_colors()))
    
    if (!(folder_path %in% current_folders)) {
      selected_folders(c(current_folders, folder_path))
      
      available_colors <- setdiff(valid_colors, used_colors)
      if (length(available_colors) == 0) available_colors <- valid_colors
      new_color <- sample(available_colors, 1)
      
      folder_colors(c(folder_colors(), setNames(list(new_color), folder_path)))
    }
    
    show("loading")
    image_files <- list.files(folder_path, pattern = "\\.jpg$", full.names = TRUE, ignore.case = TRUE)
    addResourcePath("images", folder_path)
    
    withProgress(message = "Extracting image metadata...", value = 0, session = session, {
      n <- length(image_files)
      exif_data_list <- future_lapply(seq_along(image_files), function(i) {
        exifr::read_exif(image_files[i], tags = c("FileName", "GPSLongitude", "GPSLatitude", "GPSAltitude", "DateTimeOriginal"))
      })
      for (i in seq_along(exif_data_list)) {
        incProgress(1/n)
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
  
  # Render dynamic checkboxes + ✖ remove buttons
  output$folder_checkboxes <- renderUI({
    req(selected_folders())
    folders <- selected_folders()
    checked <- checked_folders()
    
    lapply(folders, function(folder) {
      fid <- gsub("[^a-zA-Z0-9]", "_", folder)
      fluidRow(
        column(1, checkboxInput(paste0("chk_", fid), NULL, value = folder %in% checked)),
        column(9, tags$label(basename(folder))),
        column(2, actionButton(paste0("rm_", fid), "✖", class = "btn-xs btn-danger"))
      )
    }) %>% tagList()
  })
  
  # Compute checked folders based on inputs
  checked_folders <- reactive({
    req(selected_folders())
    folders <- selected_folders()
    Filter(function(f) input[[paste0("chk_", gsub("[^a-zA-Z0-9]", "_", f))]] %||% FALSE, folders)
  })
  
  # Display tables for checked folders
  output$folder_tables <- renderUI({
    req(checked_folders())
    data_list <- images_data_list()
    
    lapply(checked_folders(), function(folder) {
      if (!is.null(data_list[[folder]])) {
        tagList(
          h5(basename(folder)),
          DTOutput(paste0("table_", digest::digest(folder)))
        )
      }
    }) %>% tagList()
  })
  
  observe({
    req(checked_folders())
    data_list <- images_data_list()
    
    for (folder in checked_folders()) {
      local({
        f <- folder
        output_id <- paste0("table_", digest::digest(f))
        output[[output_id]] <- renderDT({
          req(!is.null(data_list[[f]]))
          df <- data_list[[f]] %>%
            sf::st_drop_geometry() %>%
            dplyr::select(FileName, DateTimeOriginal, GPSLongitude, GPSLatitude)
          datatable(df, options = list(pageLength = 5), class = "compact")
        })
      })
    }
  })
  
  # Remove folders when ✖ is clicked
  observe({
    for (folder in selected_folders()) {
      local({
        f <- folder
        fid <- paste0("rm_", gsub("[^a-zA-Z0-9]", "_", f))
        observeEvent(input[[fid]], {
          selected_folders(setdiff(selected_folders(), f))
          colors <- folder_colors(); colors[[f]] <- NULL; folder_colors(colors)
          dat <- images_data_list(); dat[[f]] <- NULL; images_data_list(dat)
          leafletProxy("map") %>% clearGroup(f)
        }, ignoreInit = TRUE)
      })
    }
  })
  
  # Update base map
  observeEvent(input$map_type, {
    leafletProxy("map") %>%
      clearTiles() %>%
      addProviderTiles(if (input$map_type == "Esri") providers$Esri.WorldImagery else providers$OpenStreetMap)
  })
  
  # Add markers and lines
  observe({
    req(selected_folders())
    
    all_coords <- NULL
    
    for (folder in selected_folders()) {
      folder_data <- images_data_list()[[folder]]
      folder_color <- folder_colors()[[folder]]
      if (!is.null(folder_data)) {
        coords <- sf::st_coordinates(folder_data)
        all_coords <- rbind(all_coords, coords)
        
        leafletProxy("map") %>%
          addCircleMarkers(
            lng = coords[, 1],
            lat = coords[, 2],
            popup = paste0(
              "<b>Image:</b> ", folder_data$FileName, "<br>",
              "<b>Date:</b> ", folder_data$DateTimeOriginal, "<br>",
              paste0("<img src='images/", folder_data$SourceFile, "' width='300'>")
            ),
            radius = input$marker_size,
            stroke = FALSE,
            color = folder_color,
            fillColor = folder_color,
            fillOpacity = 1,
            group = folder
          )
        
        if (input$show_path) {
          leafletProxy("map") %>%
            addPolylines(
              lng = coords[, 1], lat = coords[, 2],
              color = folder_color, weight = 2, opacity = 0.8,
              group = folder
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
  
  # Folder color legend
  output$folder_legend <- renderUI({
    req(selected_folders())
    folder_colors_list <- folder_colors()
    lapply(selected_folders(), function(folder) {
      color <- folder_colors_list[[folder]]
      selectInput(
        inputId = paste0("color_", gsub("[^a-zA-Z0-9]", "_", folder)),
        label = basename(folder),
        choices = valid_colors,
        selected = color
      )
    }) %>% tagList()
  })
  
  observe({
    req(selected_folders())
    colors <- folder_colors()
    for (folder in selected_folders()) {
      input_id <- paste0("color_", gsub("[^a-zA-Z0-9]", "_", folder))
      new_color <- input[[input_id]]
      if (!is.null(new_color) && new_color %in% valid_colors) {
        colors[[folder]] <- new_color
      }
    }
    folder_colors(colors)
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(maxZoom = 22)) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
}
