library(dplyr)
library(rgdal)
library(raster)
library(magrittr)
library(ggplot2)

# Import emerging water trends
GRACE <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/GRACE_0d05.tif")

# Part 1: Convert WRI aqueduct basin shapefile data to continuous indexes for water stress and flooding

# Load WRI 2019 release of baseline water stress
BWS_2019 <- raster("Z:/2.active_projects/Xander/! GIS_files/Aqueduct/2019_release/BWS_score.tif")

Arid_LowWaterUse <- BWS_2019
Arid_LowWaterUse[BWS_2019 == 5] <- 1
Arid_LowWaterUse[BWS_2019 < 5] <- 0
plot(Arid_LowWaterUse)

# Load WRI 2019 release of riverine flooding risk
RFR_2019 <- raster("Z:/2.active_projects/Xander/! GIS_files/Aqueduct/2019_release/RFR_score.tif")

RFR_2019[BWS_2019 == 5] <- NA # removing arid/low water use areas from analysis
BWS_2019[BWS_2019 == 5] <- NA # removing arid/low water use areas from analysis

# Create water stress modification raster from emerging water trend (drying increases, wetting decreases)
WaterStress_mod <- GRACE
WaterStress_mod[GRACE <= -1.5] <- 2
WaterStress_mod[GRACE > -1.5 & GRACE <= -0.5] <- (WaterStress_mod[GRACE > -1.5 & GRACE <= -0.5]+0.5)/(-0.5)
WaterStress_mod[GRACE > -0.5 & GRACE <= 0.5] <- 0
WaterStress_mod[GRACE > 0.5 & GRACE <= 1.5] <- (WaterStress_mod[GRACE > 0.5 & GRACE <= 1.5]-0.5)/(-0.5)
WaterStress_mod[GRACE > 1.5] <- -2
plot(WaterStress_mod)

# Create flooding modification raster from emerging water trend (wetting increases, drying decreases) 
## just the negative of the water stress modifier
Flooding_mod <- GRACE
Flooding_mod <- (-1)*WaterStress_mod
plot(Flooding_mod)

# Modify current water stress by water stress modifier
WaterStress.mod <- BWS_2019 + WaterStress_mod
WaterStress.mod[WaterStress.mod < 0] <- 0
plot(BWS_2019, main = "bws current")
plot(WaterStress.mod, main = "bws modified")

# Modify current flooding by flooding modifier
Flooding.mod <- RFR_2019 + Flooding_mod
Flooding.mod[Flooding.mod < 0] <- 0
plot(RFR_2019, main = "flooding current")
plot(Flooding.mod, main = "flooding modified")

# export modified water stress and flooding rasters
writeRaster(Flooding.mod, 
            filename="Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/GRACEmod_FLOODS.tif", 
            format="GTiff", overwrite=TRUE)
writeRaster(WaterStress.mod, 
            filename="Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/GRACEmod_STRESS.tif", 
            format="GTiff", overwrite=TRUE)
writeRaster(Arid_LowWaterUse, 
            filename="Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/Arid_LowWaterUse.tif", 
            format="GTiff", overwrite=TRUE)


