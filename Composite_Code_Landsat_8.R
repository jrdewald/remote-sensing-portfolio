# Set the working directory for Summer_Months NDVI files
setwd("H:/NOMAS/Clipped_NDVI_Layers/Summer_Months")

# List all files with .tif extension
Summer_Months_files <- list.files(pattern = "\\.tif$")

# Extract the year from the file name (assuming year is in the first 4 characters)
file_years <- as.numeric(substring(Summer_Months_files, 1, 4))

# Split files into pre-2012 and 2012 and later
pre_2012_files <- Summer_Months_files[file_years < 2013]
post_2017_files <- Summer_Months_files[file_years >= 2017]

# Load necessary libraries
library(raster)
library(rgdal)

# Define the input and output directories
input_dir <- "H:/NOMAS/Clipped_NDVI_Layers/Summer_Months"
output_dir <- "H:/NOMAS/Clipped_NDVI_Layers/Summer_Months_Clipped"


# Read the shapefile for constraining the extent
extent_shp <- rgdal::readOGR("H:/NOMAS/Shapefiles/NYC_no_Water.shp")

# List of Fmask files with full paths
Fmask_dir <- "H:/NOMAS/Raw_Data/Landsat8/Clipped_Fmask_Layers"
Fmask_files <- list.files(Fmask_dir, pattern = "\\.tif$", full.names = TRUE)

# Define batch size for processing in groups
batch_size <- 5  # Adjust based on memory availability

# Split post_2017_files into smaller batches
batches <- split(post_2017_files, ceiling(seq_along(post_2017_files) / batch_size))

# Process each batch of files separately
for (batch in batches) {
  for (file in batch) {
    
    #TEST REMOVE WHEN DONE
    file<-post_2017_files[[2]]
    full_path <- file.path(input_dir, file)
    result <- tryCatch({
      
      # Read the raster file
      raster_layer <- raster(full_path)
      
      # Apply the scaling function
      raster_layer <- raster_layer / 10000.0
      
      # Clip and mask the raster using the extent shapefile
      raster_layer <- crop(raster_layer, extent_shp)
      raster_layer <- mask(raster_layer, extent_shp)
      
      # Set pixel values below or equal to 0 to zero
      raster_layer[raster_layer <= 0] <- 0
      raster_layer[raster_layer == 2] <- 0
      
      # Count the number of values equal to 1, ignoring NA values
      num_ones <- sum(raster_layer[] == 1, na.rm = TRUE)
      
      # Check for matching Fmask files
      if (num_ones >= 1) {
        Date <- substr(file, 1, 8)
        matched_entries <- Fmask_files[sapply(Fmask_files, function(x) substr(basename(x), 39, 46) == Date)]
        
        if (length(matched_entries) > 0) {
          fmask_layer <- raster(matched_entries[1])  # Assuming only one match
          
          # Apply the mask to raster_layer, setting the corresponding pixel values to NA
          raster_layer[fmask_layer == 1] <- 0
        } else {
          message(sprintf("No matching Fmask file found for '%s'", file))
        }
      }
      
      # Final mask and save operation
      raster_layer <- mask(raster_layer, extent_shp)
      output_path <- file.path(output_dir, paste0("Clipped_", basename(file)))
      writeRaster(raster_layer, filename = output_path, format = "GTiff", overwrite = TRUE)
      
    }, error = function(e) {
      # Print the error message and skip to the next file
      message(sprintf("Skipping file '%s' due to error: %s", file, e$message))
    })
  }
  
  # Run garbage collection after each batch to free memory
  gc()
}





# Set the working directory for Summer_Months NDVI files
setwd("H:/NOMAS/Clipped_NDVI_Layers/Summer_Months_Clipped")
# List all files with .tif extension
Summer_Months_files <- list.files(pattern = "\\.tif$")
# Extract the year from the file name (first 4 characters)
years <- as.integer(substr(Summer_Months_files, 9, 12))
# Create a full path for each file
file_paths <- file.path(getwd(), Summer_Months_files)

# Create a raster stack from the list of files
raster_stack <- stack(file_paths)
# Create an empty raster stack for the yearly composites
yearly_composites <- stack()

# Loop through each unique year
for (year in unique(years)) {
  # Subset the layers corresponding to the current year
  year_layers <- raster_stack[[which(years == year)]]
  
  if (nlayers(year_layers) > 0) {
    # Calculate the maximum value, ignoring NA values
    year_max <- calc(year_layers, fun = function(x) max(x, na.rm = TRUE))
    
    # Add the yearly composite to the stack
    yearly_composites <- stack(yearly_composites, year_max)
  } else {
    warning(paste("No layers found for year:", year))
  }
}

# Set the working directory for saving Summer_Composites_Max
setwd("H:/NOMAS/Clipped_NDVI_Layers/Summer_Composites_Max")

# Save the yearly composites to disk
for (i in 1:nlayers(yearly_composites)) {
  writeRaster(yearly_composites[[i]], 
              filename = paste("YearlyComposite_", unique(years)[i], ".tif", sep = ""),
              format = "GTiff", overwrite = TRUE)
}

# Print a message indicating the completion
cat("Yearly composites created and saved.\n")
