# Remote Sensing & Geospatial Data Engineering Portfolio

This repository contains selected examples of my work in remote sensing, geospatial analysis, and geospatial data engineering for environmental applications.

My background is in applied environmental science and remote sensing, with experience building reproducible and scalable workflows for processing large satellite and raster datasets (e.g., Landsat and Sentinel). My work includes generating NDVI composites, quantifying changes in greenness over time, integrating spatial and longitudinal datasets, and developing cloud-based and local geospatial pipelines using R and Google Earth Engine.

Each script in this repository is designed to demonstrate specific technical and analytical capabilities.

---

## Code Examples

### 1. NDVI Temporal Attribution Pipeline (Miami)
**File:** `Connect_Geocode_to_NDVI_data_Miami.R`

This script demonstrates a workflow for linking remotely sensed NDVI data to longitudinal residential address histories. Participant relocation events are expanded temporally, and NDVI values are assigned based on spatial units (FIPS codes) for each season and year.

Key features include:
- Temporal expansion of residence intervals
- Partial-season weighting for participants who relocate during a season
- Assignment of seasonal NDVI exposure per participant
- Calculation of average and cumulative NDVI exposure across the study period

This approach enables the assessment of long-term environmental exposure while accounting for residential mobility.

---

### 2. Remotely Sensed Ecological Index (RSEI) Calculation
**File:** `RSEI_Calculation_Normalized_PCA.R`

This script calculates a Remotely Sensed Ecological Index (RSEI) for Miami-Dade County using indices derived from Sentinel-2 and Landsat 8 imagery. The index is generated using a normalized Principal Component Analysis (PCA) approach to integrate multiple environmental indicators into a single composite metric.

Sentinel-2 indices used in this workflow are generated using the Google Earth Engine script described below.

---

### 3. Landsat 8 Summer NDVI Composites
**File:** `Composite_Code_Landsat_8.R`

This script preprocesses Landsat 8 summer NDVI layers by:
- Scaling reflectance values
- Spatially masking data to a defined study extent
- Removing cloud-contaminated pixels using Fmask outputs
- Generating yearly maximum NDVI composites

The workflow is designed for batch processing and reproducible generation of long-term NDVI time series.

---

### 4. Google Earth Engine: Sentinel-2 Index Generation
**File:** `Sentinel2_RSEI_GEE.js`

This Google Earth Engine script generates and exports Sentinel-2 spectral indices required for the RSEI calculation. The script demonstrates cloud masking, index calculation, and preparation of analysis-ready outputs for downstream processing in R.

---

## Notes
- All scripts have been anonymized and refactored for public sharing.
- Human-subject data, proprietary datasets, and restricted assets have been removed.
- Example workflows are representative of methods used in peer-reviewed research and applied environmental analyses.
