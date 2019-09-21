library(raster)
library(dplyr)
library(magrittr)

# load GRACE data
GRACE_0d05res <- raster("Z:/2.active_projects/Xander/! GIS_files/GRACE/GRACE_coredata.tif")

# load cell sizes
CellArea <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/WGS84_cellArea_0d05res.tif")

# create global coverage raster with constant value
Wrld <- raster(CellArea)
Wrld[] <- 1

WorldVolumetricTrend <- zonal(GRACE_0d05res*CellArea, Wrld, "sum") # world total volumetric flow rate... should be 0 if there is conservation of mass
WorldArea <- zonal(CellArea, Wrld, "sum") # world surface area, accurate to 99.99874%
WorldDepthTrend <- WorldVolumetricTrend/WorldArea
# > -0.0008606349 cm/yr
# > -8.606349 micrometers/yr

# Total aggregate movement in water observed by GRACE
Abs.Vol.Trends <- (1/100000)*abs(GRACE_0d05res)*CellArea # in km/yr
VolumetricMovementObserved <- zonal(Abs.Vol.Trends, Wrld, "sum") # total aggregate movement detected, in either direction
# 2309.8 km3/yr of water (aggregated TWS) redistributed

# Volumetric global change in storage (error if assuming complete water balance)
Volumetric.Error <- (1/100000)*WorldVolumetricTrend # converting from (cm*km2/yr) to (km3/yr)
# 4.3898 km3/yr error term
# 4.3898/2309.8 ~= 0.0019
# Therefore, non-convergence magnitudeis 0.2% of all water movement observed; thus water storage is 99.8% balanced in this analysis 

# determine total cell inflows
Inflows <- GRACE_0d05res
Inflows[GRACE_0d05res[] < 0] <- 0
Inflows_total <- zonal((1/100000)*Inflows*CellArea, Wrld, "sum") # total cell inflows

# determine total cell inflows
Outflows <- GRACE_0d05res
Outflows[GRACE_0d05res[] > 0] <- 0
Outflows_total <- zonal((1/100000)*Outflows*CellArea, Wrld, "sum") # total cell inflows
