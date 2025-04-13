library(shiny)
library(shinyFiles)
library(colourpicker)
library(leaflet)
library(leaflet.extras)
library(shinyjs)
library(sf)
library(terra)
library(exifr)
library(magick)
library(DT)
library(dplyr)
library(future.apply)
plan(multisession)


# Define file paths
volumes <- c(
  Home = fs::path_home(),
  "C:" = "C:/", 
  "D:" = "D:/"
)

# Helper: null coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Detect external drives
external_drives <- function() {
  os <- .Platform$OS.type
  if (os == "windows") {
    drives <- system("wmic logicaldisk get name", intern = TRUE)
    drives <- gsub("\\s", "", drives)  
    drives <- drives[grepl(":", drives)]  
    names(drives) <- drives
    return(drives)
  } else if (os == "unix") {
    drives <- list.dirs("/Volumes", recursive = FALSE)
    if (dir.exists("/media")) {
      drives <- c(drives, list.dirs("/media", recursive = FALSE))
    }
    names(drives) <- basename(drives)
    return(drives)
  }
  return(NULL)
}

# Add external drives to `volumes`
volumes <- c(volumes, external_drives())