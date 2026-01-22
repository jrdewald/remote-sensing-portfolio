
# remote-sensing-portfolio

This repository contains selected examples of my work in remote sensing, geospatial analysis, and data engineering for environmental applications.

My background is in applied environmental science and remote sensing, with experience building reproducible workflows for processing large satellite and raster datasets (e.g., Landsat, Sentinel), developing NDVI composites, measuring changes in greenness over time, and scalable geospatial pipelines using R, and Google Earth Engine.

Brief descriptions of the different coding files in this example repository:

1. Connect Geocode to NDVI data (Miami).R
   
  This code was used for a study that attempted to see how changes in greenness in a participant's life might result in changes to their overall health. Address histories were available for all participants, documenting relocations across the study period. NDVI values were assigned based on Federal Information Processing Standards (FIPS) codes. A singular NDVI value per participant per season per year was assigned by the code. For participants relocating during a season, a weighted average of NDVI values from previous and current locations was computed, reflecting exposure duration. To assess overall environmental exposure, the total study inclusion days for each participant were calculated. Subsequently, the average and cumulative NDVI values across the study duration were determined by the code, providing insights into long-term exposure levels

2. RSEI Calculation Script (Normalized PCA Method).R

  This code uses indices derived from Sentinel-2 and Landsat 8 data to create a Remotely Sensed Ecological Index (RSEI) for Miami-Dade County. This technique uses a Principle Component Analysis (PCA) to create the index.  The Sentinel-2 data was created by the GEE code in item #5.

3. Composite_Code_Landsat_8.R

  This script preprocesses Landsat 8 summer NDVI layers by scaling reflectance values, spatially masking to a study extent, removing all areas that are identified as clouds from a previous Fmask output before generating the final yearly maximum NDVI composites.

4. GEE code for downloading Sentinel 2 data for RSI calculation

  This Google Earth Engine Script works to create and download the nessecary Sentinel-2 indices required for the RSEI code in item #2.


