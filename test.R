
image = read_exif("/Users/wsedgwick/Desktop/python_work/martin_work/data/flamingos/MAX_0226.JPG")

image


# Read EXIF metadata from a folder of images
image_files <- list.files("/Users/wsedgwick/Desktop/python_work/martin_work/data/flamingos/", pattern = "\\.JPG$", full.names = TRUE)
test_image_files <- list.files("/Users/wsedgwick/Desktop/drone_tool_shiny", pattern = "\\.JPG$", full.names = TRUE)

# Extract EXIF metadata (including GPS coordinates)
exif_data <- exifr::read_exif(test_image_files)

# View the first few rows
head(exif_data)


