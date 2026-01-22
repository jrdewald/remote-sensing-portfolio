# ============================================================
# Landsat 8 Summer NDVI Processing & Yearly Max Composites
# ============================================================
# Author: Julius R. Dewald
# Description:
#   This script preprocesses Landsat 8 summer NDVI layers by:
#   - Scaling reflectance values
#   - Spatially masking to a study extent
#   - Applying Fmask cloud masking (fmask files were created in another code)
#   - Generating yearly maximum NDVI composites
#
# Notes:
#   - Input data paths are user-defined
#   - Human-subject and proprietary data removed
# ============================================================

# -----------------------------
# Load libraries
# -----------------------------
library(raster)
library(rgdal)

# -----------------------------
# User-defined paths
# -----------------------------
input_ndvi_dir  <- "data/ndvi/summer_layers"
input_fmask_dir <- "data/fmask"
extent_shp_path <- "data/shapefiles/study_extent.shp"

processed_dir   <- "output/ndvi_processed"
composite_dir   <- "output/ndvi_yearly_composites"

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(composite_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Load spatial extent
# -----------------------------
study_extent <- readOGR(extent_shp_path)

# -----------------------------
# List NDVI and Fmask files
# -----------------------------
ndvi_files  <- list.files(input_ndvi_dir, pattern = "\\.tif$", full.names = TRUE)
fmask_files <- list.files(input_fmask_dir, pattern = "\\.tif$", full.names = TRUE)

# -----------------------------
# Helper function: process NDVI
# -----------------------------
process_ndvi_layer <- function(ndvi_path, extent_shp, fmask_files) {

  ndvi <- raster(ndvi_path) / 10000
  ndvi <- crop(ndvi, extent_shp)
  ndvi <- mask(ndvi, extent_shp)

  ndvi[ndvi <= 0] <- 0
  ndvi[ndvi == 2] <- 0

  valid_pixel_count <- sum(ndvi[] == 1, na.rm = TRUE)

  if (valid_pixel_count > 0) {
    acquisition_date <- substr(basename(ndvi_path), 1, 8)

    matched_fmask <- fmask_files[
      sapply(fmask_files, function(x)
        substr(basename(x), 39, 46) == acquisition_date
      )
    ]

    if (length(matched_fmask) > 0) {
      fmask <- raster(matched_fmask[1])
      ndvi[fmask == 1] <- 0
    }
  }

  return(mask(ndvi, extent_shp))
}

# -----------------------------
# Batch processing
# -----------------------------
batch_size <- 5
ndvi_batches <- split(ndvi_files,
                      ceiling(seq_along(ndvi_files) / batch_size))

for (batch in ndvi_batches) {

  for (ndvi_path in batch) {
    tryCatch({

      processed_ndvi <- process_ndvi_layer(
        ndvi_path,
        study_extent,
        fmask_files
      )

      output_path <- file.path(
        processed_dir,
        paste0("processed_", basename(ndvi_path))
      )

      writeRaster(processed_ndvi,
                  filename = output_path,
                  format = "GTiff",
                  overwrite = TRUE)

    }, error = function(e) {
      message(sprintf(
        "Skipping %s due to error: %s",
        basename(ndvi_path),
        e$message
      ))
    })
  }

  gc()
}

# -----------------------------
# Create yearly max composites
# -----------------------------
processed_files <- list.files(
  processed_dir,
  pattern = "\\.tif$",
  full.names = TRUE
)

years <- as.integer(substr(basename(processed_files), 11, 14))
ndvi_stack <- stack(processed_files)

unique_years <- sort(unique(years))

for (yr in unique_years) {

  yearly_layers <- ndvi_stack[[years == yr]]

  if (nlayers(yearly_layers) > 0) {

    yearly_max <- calc(
      yearly_layers,
      fun = function(x) max(x, na.rm = TRUE)
    )

    writeRaster(
      yearly_max,
      filename = file.path(
        composite_dir,
        paste0("ndvi_max_", yr, ".tif")
      ),
      format = "GTiff",
      overwrite = TRUE
    )
  }
}

message("Yearly NDVI composites successfully generated.")
