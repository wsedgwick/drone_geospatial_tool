library(exifr)
# system.time({
#   exifr::read_exif(list.files("D:\pix4d_processing\latam\chile\2025\peatland 1\20250227 peatland 1 rgb RX1\F1", full.names = TRUE)[1:50],
#                    tags = c("FileName", "GPSLongitude", "GPSLatitude", "GPSAltitude", "DateTimeOriginal"))
# })



image_dir <- "D:/pix4d_processing/latam/chile/2025/peatland 1/20250227 peatland 1 rgb RX1/F1"
image_files <- list.files(image_dir, full.names = TRUE)
length(image_files)  # Check how many images it sees

system.time({
  exifr::read_exif(image_files[1:50], tags = c("FileName", "GPSLongitude", "GPSLatitude", "GPSAltitude", "DateTimeOriginal"))
})



folder_path <- "E:/carbon/2025 04 LLP surveys/2025 04 10/evo/greenwood big woods"
trinity_path <- "D:/pix4d_processing/usa/2025 04 LLP surveys/greenwood big woods/RX1RII"

# Recursively read EXIF from all image files (you can set recursive=FALSE if needed)
evo_exif_data <- read_exif(folder_path, recursive = TRUE)


# Confirm path is correct
trinity_path

# List JPGs in that folder and subfolders
image_files <- list.files(trinity_path, pattern = "\\.jpe?g$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)[1]


# Check the result
length(image_files)        # How many files found?
head(image_files, 3)       # Show a few

trinity_exif_data <- read_exif(image_files)

colnames(evo_exif_data)
colnames(image_files)
