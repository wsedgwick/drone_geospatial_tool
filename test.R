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
