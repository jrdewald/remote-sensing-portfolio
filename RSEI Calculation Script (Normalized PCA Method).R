# =========================================================
#   RSEI Calculation Script (Normalized PCA Method)
# =========================================================

library(terra)   # raster handling
library(dplyr)   # data wrangling
library(readr)
library(sf)      # shapefile handling

# ---------------------------------------------------------
# 1. Load raster files
	# ---------------------------------------------------------
				NDSI    <- rast("Path to Sentinel 2 data/_S2_TOA_NDSI.tif")   
				NDVI    <- rast("Path to Sentinel 2 data/_S2_TOA_NDVI.tif") 
				Wetness <- rast("Path to Sentinel 2 data/_S2_TOA_Wetness.tif")     
				Heat    <- rast("Path to Landsat 8 heat data/LST_Landsat8_HEAT_Resample.tif")
# ---------------------------------------------------------
# 2. Load and project study area shapefile
# ---------------------------------------------------------
				
			Study_Area <- st_read("Shapefile of Study area")
			Study_Area <- st_transform(Study_Area, crs(NDVI))  # ensure CRS match

# ---------------------------------------------------------
# 3. Align rasters to NDVI reference
# ---------------------------------------------------------
			Heat <- extend(Heat, ext(NDVI))
			Heat <- resample(Heat, NDVI, method = "bilinear")  # continuous variable
			
			NDSI    <- resample(NDSI, NDVI, method = "bilinear")
			Wetness <- resample(Wetness, NDVI, method = "bilinear")
			diversity<-resample(diversity, NDVI, method = "bilinear")
			
			# Crop and mask to study area
			NDSI    <- crop(NDSI, Study_Area) |> mask(Study_Area)
			NDVI    <- crop(NDVI, Study_Area) |> mask(Study_Area)
			Wetness <- crop(Wetness, Study_Area) |> mask(Study_Area)
			# Remove infinite values
			Heat[!is.finite(Heat)] <- NA
		
# ---------------------------------------------------------
# 4. Normalize each indicator to 0–1 range  (fixed)
# ---------------------------------------------------------
			normalize <- function(x) {
			  mm <- minmax(x)
			  (x - mm[1, 1]) / (mm[2, 1] - mm[1, 1])
			}
			
			NDVI_norm    <- normalize(NDVI)
			NDSI_norm    <- normalize(NDSI)
			Wetness_norm <- normalize(Wetness)
			Heat_norm    <- normalize(Heat)

# ---------------------------------------------------------
# 5. Stack normalized rasters
# ---------------------------------------------------------
			stack_norm <- c(NDVI_norm, NDSI_norm, diversity_norm, Heat_norm)
			names(stack_norm) <- c("NDVI", "NDSI", "Diversity", "Heat")

# ---------------------------------------------------------
# 6. Perform PCA on normalized values
# ---------------------------------------------------------
			stack_df <- na.omit(as.data.frame(stack_norm, xy = FALSE))
			pca_result <- prcomp(stack_df, center = TRUE, scale. = FALSE)  # scale.=FALSE since already normalized

# View PCA summary
			summary(pca_result)

# Extract PC1 loadings
			loadings <- pca_result$rotation[, 1]
			print(loadings)

# ---------------------------------------------------------
# 7. Apply PCA model to raster stack
# ---------------------------------------------------------
			pca_raster <- predict(stack_norm, pca_result, index = 1)

# ---------------------------------------------------------
# 6. Transform PC1 into final RSEI
#    b) Normalize to [0,1]
# ---------------------------------------------------------

# Normalize to [0,1]
			min_val <- as.numeric(global(pca_raster, "min", na.rm = TRUE))
			max_val <- as.numeric(global(pca_raster, "max", na.rm = TRUE))
			
			RSEI <- (pca_raster - min_val) / (max_val - min_val)

# ---------------------------------------------------------
# 8. Export final RSEI raster
# ---------------------------------------------------------
			writeRaster(
			  RSEI,
			  filename = "Path to Sentinel 2 data/_RSEI_Diversity_PCA.tif",
			  filetype = "GTiff",
			  overwrite = TRUE,
			  gdal = c("COMPRESS=LZW")
			)

