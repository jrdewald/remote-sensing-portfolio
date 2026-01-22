# remote-sensing-portfolio

This repository contains selected examples of my work in remote sensing, geospatial analysis, and data engineering for environmental applications.

My background is in applied environmental science and remote sensing, with experience building reproducible workflows for processing large satellite and raster datasets (e.g., Landsat, Sentinel), developing NDVI composites, measuring changes in greenness over time, and scalable geospatial pipelines using R, and Google Earth Engine.

Brief descriptions of the different coding files in this example repository:
  1. BiodivMap_Miami.R
     
    This code works create a biodiversity map of Miami-Dade countyby using the biodivmap R library.The biodiversity map relies on the assumption that areas with greater amounts of spectral diversity are a proxy for       biodiversity. In this code the user downloads and unzips a particular Sentinel-2 image and then masks out areas of no vegetation using NDVI, Blue, and NIR thresholds. It then creates the nessecary indices from the sentinel 2 data before performing the biodivMapR_full() function to create the final biodiversity map.

2. Connect Geocode to NDVI data (Miami).R
   
   This code was used for a study that attempted to see how changes in greenness in a participant's life might result in changes to their overall health. Address histories were available for all participants,
documenting relocations across the study period. NDVI values were assigned based on Federal Information Processing Standards (FIPS) codes. A singular NDVI value per participant per season per year was assigned by the code. For participants relocating during a season, a weighted average of NDVI values from previous and current locations was computed, reflecting exposure duration. To assess overall environmental exposure, the total study inclusion days for each participant were calculated. Subsequently, the average and cumulative NDVI values across the study duration were determined by the code, providing insights into long-term exposure levels

3. RSEI Calculation Script (Normalized PCA Method).R

  This code uses indices derived from Sentinel-2 and Landsat 8 data to create a Remotely Sensed Ecological Index (RSEI) for Miami-Dade County. This technique uses a Principle Component Analysis (PCA) to create the index.  The Sentinel-2 data was created by the GEE code

4. 
