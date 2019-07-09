library(dplyr)
library(rgdal)
library(raster)
library(magrittr)
library(ggplot2)

# Import data
# Emerging water availability trends from Rodell et al. (2018)
EmergingTrend <- raster("E:/! GIS_files/R_gis_exports/GRACE_0d05.tif")
# WRI Aqueduct HydroBASIN dataset (includes water stress and flood frequency)
WRIbasins <- readOGR(dsn = "E:/! GIS_files/Aqueduct",
                     layer = "aqueduct_global_dl_20150409")

# Convert WRI aqueduct basin shapefile data to continuous indexes for water stress and flooding
# Simplify attribute table
keepcols <- c("GU","Shape_Leng","Shape_Area","BasinID","COUNTRY","BASIN_NAME",
              "BWS", "BWS_s", "BWS_cat", "HFO", "HFO_s", "HFO_cat")
WRIbasins_clean <- WRIbasins[, keepcols, drop = FALSE]

# classify water stress into continuous index
WRIbasins_clean$BWS_scale <- ifelse(WRIbasins_clean$BWS < 0.1, (WRIbasins_clean$BWS/0.1),
                                    ifelse(WRIbasins_clean$BWS < 0.2, 1 + ((WRIbasins_clean$BWS-0.1)/0.1),
                                           ifelse(WRIbasins_clean$BWS < 0.4, 2 + ((WRIbasins_clean$BWS-0.2)/0.2),
                                                  ifelse(WRIbasins_clean$BWS < 0.8, 3 + ((WRIbasins_clean$BWS-0.4)/0.4),
                                                         ifelse(WRIbasins_clean$BWS >= 0.8, 4,
                                                                ifelse(WRIbasins_clean$BWS == Inf, 4, 0))))))
WRIbasins_clean$BWS_scale <- ifelse(WRIbasins_clean$BWS_scale > 4, 4,
                                    ifelse(WRIbasins_clean$BWS_scale < 0, 0, WRIbasins_clean$BWS_scale))

# classify flooding into continuous index
WRIbasins_clean$Flood_scale <- ifelse(WRIbasins_clean$HFO < 1, (WRIbasins_clean$HFO/1),
                                    ifelse(WRIbasins_clean$HFO < 3, 1 + ((WRIbasins_clean$HFO-2)/1),
                                           ifelse(WRIbasins_clean$HFO < 9, 2 + ((WRIbasins_clean$HFO-4)/5),
                                                  ifelse(WRIbasins_clean$HFO < 27, 3 + ((WRIbasins_clean$HFO-10)/17),
                                                         ifelse(WRIbasins_clean$HFO >= 27, 4,
                                                                ifelse(WRIbasins_clean$HFO == Inf, 4, 0))))))
WRIbasins_clean$Flood_scale <- ifelse(WRIbasins_clean$Flood_scale > 4, 4,
                                    ifelse(WRIbasins_clean$Flood_scale < 0, 0, WRIbasins_clean$Flood_scale))

# Write to new shapefile so that vector shapefile can be rasterized in QGIS
writeOGR(WRIbasins_clean, dsn="E:/! GIS_files/Aqueduct/WRI_ContinuousIndex.shp", 
         layer = "WRI_ContinuousIndex", driver="ESRI Shapefile", overwrite_layer=TRUE)

# Rasterize exported shapefile to 0.05d resolution in QGIS (optimal performance to R) for continuous flood and water stress index,
# and re-import here
WaterStress.current <- raster("E:/! GIS_files/Aqueduct/Rasterize/WaterStress_ContinuousIndex.tif")
Flooding.current <- raster("E:/! GIS_files/Aqueduct/Rasterize/Flooding_ContinuousIndex.tif")

# Create water stress modification raster from emerging water trend (drying increases, wetting decreases)
WaterStress_mod <- EmergingTrend
WaterStress_mod[EmergingTrend <= -1.5] <- 2
WaterStress_mod[EmergingTrend > -1.5 & EmergingTrend <= -0.5] <- (WaterStress_mod[EmergingTrend > -1.5 & EmergingTrend <= -0.5]+0.5)/(-0.5)
WaterStress_mod[EmergingTrend > -0.5 & EmergingTrend <= 0.5] <- 0
WaterStress_mod[EmergingTrend > 0.5 & EmergingTrend <= 1.5] <- (WaterStress_mod[EmergingTrend > 0.5 & EmergingTrend <= 1.5]-0.5)/(-0.5)
WaterStress_mod[EmergingTrend > 1.5] <- -2

# Create flooding modification raster from emerging water trend (wetting increases, drying decreases) 
## (practically, this is just the negative of the water stress modifier)
Flooding_mod <- EmergingTrend
Flooding_mod <- (-1)*WaterStress_mod

# Modify current water stress by water stress modifier
WaterStress.mod <- WaterStress.current + WaterStress_mod
WaterStress.mod[WaterStress.mod < 0] <- 0

# Modify current flooding by flooding modifier
Flooding.mod <- Flooding.current + Flooding_mod
Flooding.mod[Flooding.mod < 0] <- 0

# export modified water stress and flooding rasters
writeRaster(Flooding.mod, 
            filename="E:/! GIS_files/R_gis_exports/WRI_Flooding_mod.tif", 
            format="GTiff", overwrite=TRUE)
writeRaster(WaterStress.mod, 
            filename="E:/! GIS_files/R_gis_exports/WRI_WaterStress_mod.tif", 
            format="GTiff", overwrite=TRUE)

