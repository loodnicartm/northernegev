# northernegev
This R script performs an analysis of vegetation change in the Northern Negev region (Israel) using multi-year NDVI data from Landsat satellite imagery (from 1984-2020). The goal is to visualize spatial and temporal vegetation dynamics and detect significant trends over time.

-Key Features:
Load and process Landsat NDVI time series as raster stacks.

Calculate annual NDVI averages and explore trends over time.

Detect statistically significant changes using the Theil-Sen estimator and Mann-Kendall trend test.

Visualize spatial patterns of NDVI trends and identify areas of vegetation increase or decline.

-Technologies & Libraries Used:
raster, terra, sp, rgdal – for geospatial data handling

Kendall, trend, zyp – for time series analysis and trend detection

ggplot2, RColorBrewer, sf – for data visualization

-Applications:
This type of analysis is useful for monitoring land degradation, dryland management, and ecological or climate-related studies that require spatio-temporal NDVI trend assessments.


