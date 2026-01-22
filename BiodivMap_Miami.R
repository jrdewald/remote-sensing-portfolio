###############################################################################
# Sentinel-2 Biodiversity Mapping Pipeline
#
# Description:
# This script demonstrates a remote sensing workflow that downloads Sentinel-2
# Level-2A surface reflectance data to derive vegetation masks, spectral features,
# to create a biodiversity proxy map using SPCA and clustering-based approaches. The current 
# boundaries are set for the city of Miami FL.
#
# Notes:
# - All file paths are user-configurable
# - Credentials are expected via environment variables
# - Intended as a technical portfolio example
###############################################################################

# =============================
# 1. Load required libraries
# =============================
library(preprocS2)
library(sf)
library(zip)
library(biodivMapR)
library(spinR)

# =============================
# 2. User configuration
# =============================

# ---- File paths (EDIT THESE) ----
project_dir <- "path/to/project_directory"
s2_output_dir <- file.path(project_dir, "sentinel2_data")
biodiv_output_dir <- file.path(project_dir, "biodivMapR_outputs")

dir.create(s2_output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(biodiv_output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Sentinel-2 parameters ----
acquisition_date <- "2018-12-22"
site_name <- "S2_Miami_Example"

# Study area bounding box (UTM Zone 17N)
bbox <- st_bbox(c(
  xmin = 526642.600432,
  ymin = 2804359.968110,
  xmax = 589374.063815,
  ymax = 2873083.974621
), crs = 32617)

# Sentinel-2 tiling grid (publicly available)
s2_tiling_grid <- "Sentinel-2_tiling_grid.kml"

# =============================
# 3. Authentication (via env vars)
# =============================

# Expected environment variables:
#   CDSE_ID
#   CDSE_SECRET
#
# Example (~/.Renviron):
#   CDSE_ID="sh-XXXX..."
#   CDSE_SECRET="XXXX..."

authentication <- list(
  id  = Sys.getenv("CDSE_ID"),
  pwd = Sys.getenv("CDSE_SECRET")
)

# =============================
# 4. Sentinel-2 data acquisition
# =============================

s2_files <- get_s2_raster(
  bbox = bbox,
  geomAcq = TRUE,
  path_S2tilinggrid = s2_tiling_grid,
  datetime = acquisition_date,
  output_dir = s2_output_dir,
  siteName = site_name,
  overwrite = TRUE
)

# Extract key outputs
Refl_L2A         <- s2_files$Refl_L2A
vegetation_mask <- s2_files$vegetation_mask
SCL              <- s2_files$provider_mask
geometryAcq      <- s2_files$geometryAcquisition

# =============================
# 5. Ancillary plot data (example)
# =============================

plot_dir <- file.path(project_dir, "ground_data")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

zip_url <- "https://gitlab.com/jbferet/myshareddata/-/raw/master/biodivMapR_S2_Sample/VECTOR/S2A_T33NUD_Plots.zip"
zip_path <- file.path(plot_dir, "plots.zip")

download.file(zip_url, zip_path, mode = "wb")
unzip(zip_path, exdir = plot_dir)
unlink(zip_path)

# =============================
# 6. Spectral configuration
# =============================

hdr_path <- system.file(
  "extdata", "HDR", "SENTINEL_2.hdr",
  package = "biodivMapR"
)

SensorBands <- read_ENVI_header(hdr_path)$wavelength

# =============================
# 7. Radiometric filtering
# =============================

spca_dir <- file.path(biodiv_output_dir, "SPCA")
dir.create(spca_dir, showWarnings = FALSE, recursive = TRUE)

mask_path <- radiometric_filtering(
  input_raster_path = Refl_L2A,
  input_rast_wl = SensorBands,
  output_dir = spca_dir,
  NDVI_Thresh = 0.45,
  Blue_Thresh = 500,
  NIR_Thresh  = 1500
)

# =============================
# 8. Spectral transformation (SPCA)
# =============================

PCA_Output <- perform_PCA(
  input_raster_path = Refl_L2A,
  input_rast_wl = SensorBands,
  output_dir = spca_dir,
  Continuum_Removal = TRUE,
  input_mask_path = mask_path,
  maxRows = 1000
)

# =============================
# 9. Spectral index generation
# =============================

si_dir <- file.path(biodiv_output_dir, "Spectral_Indices")
dir.create(si_dir, showWarnings = FALSE, recursive = TRUE)

SI_list <- c("NDVI", "LAI_SAVI", "CR_SWIR", "RE_NDVI", "mNDVI705")

SI_path <- spectralindices_from_raster(
  input_raster_path = Refl_L2A,
  input_rast_wl = SensorBands,
  output_dir = si_dir,
  SI_list = SI_list,
  input_mask_path = mask_path
)

# Update mask using IQR filtering
mask_IQR <- file.path(si_dir, "Mask_update_IQR.tif")

compute_mask_IQR(
  input_raster_path = SI_path,
  output_mask_path = mask_IQR,
  input_mask_path = mask_path
)

# =============================
# 10. Biodiversity proxy mapping
# =============================

biodiv_dir <- file.path(biodiv_output_dir, "Biodiversity_Results")
dir.create(biodiv_dir, showWarnings = FALSE, recursive = TRUE)

selectedPC <- c(1, 2, 3, 4, 5, 6)

biodiv_results <- biodivMapR_full(
  input_raster_path = PCA_Output$PCA_Files$PCA,
  input_mask_path = mask_path,
  output_dir = biodiv_dir,
  SelectBands = selectedPC,
  window_size = 3,
  nbclusters = 50,
  maxRows = 1000,
  nbCPU = 8 #ensure you change this to the number of CPUs for your current computer
)

# =============================
# End of pipeline
# =============================
