library(exifr)
system.time({
  exifr::read_exif(list.files('/Users/wsedgwick/Desktop/drone images for others/2025 Peru Flamingos/106FTASK', full.names = TRUE)[1:50],
                   tags = c("FileName", "GPSLongitude", "GPSLatitude", "GPSAltitude", "DateTimeOriginal"))
})
