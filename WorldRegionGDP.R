library(ggplot2)
library(scales)
library(dplyr)
library(sp)
library(raster)
library(rgdal)
library(e1071)

#Import Rodell et al. (2018) emerging freshwater availability trend as done in previous scripts
EmergingTrend <- raster("E:/! Xander/! Research/GIS_files/R_gis_exports/GRACE_0d05.tif")

# Import GDP (PPP) raw raster from Kummu et al. (2018)
GDP.ppp <- raster("E:/! Xander/! Research/GIS_files/GDP/Kummu/2015_GDP_PPP_2015.tif")

# Import population raster used in previous scripts
Population <- raster("E:/! Xander/! Research/GIS_files/R_gis_exports/POP_2015_0d05.tif")

# Resample the gridded GDP dataset to arrive at 0.05d resolution (can be executed faster in QGIS or by using multiple cores)
GDP <- raster::aggregate(x = GDP, fact = 6, fun = "sum", expand = TRUE)
# write raster so resampling isn't required to rerun the code
writeRaster(GDP,
            filename="E:/! Xander/! Research/GIS_files/R_gis_exports/Kummu_GDP_0d05.tif",
            format="GTiff", overwrite=TRUE)

# Import country shapefile IDed based on MAgPIE world economic region
MAgPIE.regions <- readOGR(dsn = "E:/! Xander/! Research/GIS_files/Admin0_countries/MagPIE_regions",
                          layer = "MAgPIE_regions")
# create unique ID per MAgPIE region
MAgPIE.regions$ID <- seq(1, nrow(MAgPIE.regions), 1)
# Write OGR so that rasterization can be performed in QGIS (faster than rasterizing in Rstudio)
writeOGR(MAgPIE.regions, dsn="E:/! Xander/! Research/GIS_files/Admin0_countries/MagPIE_regions/MAgPIE_ID.shp", 
         layer = "MAgPIE_ID", driver="ESRI Shapefile", overwrite_layer=TRUE)
# Import rasterized MAgPIE regions of the world
MAgPIE_ras <- raster("E:/! Xander/! Research/GIS_files/WorldRegions/MAgPIE_worldregions.tif")

## Reclassify emerging trends to 0.1 increment bins
reclassRanges <- data.frame(low = seq(-40, 6.0, 0.1), high = seq(-39.9, 6.1, 0.1), ReCLASS = seq(1:461))
EmergingTrend_Reclass <- reclassify(EmergingTrend, reclassRanges)



# Creating a function that takes the ID of the world region and returns the total GDP in region
GDP.func <- function(WorldRegion_ID){
  ID.Raster <- raster(EmergingTrend)
  ID.Raster[] <- GDP[]
  ID.Raster[MAgPIE_ras != WorldRegion_ID] <- NA
  ID.Raster.Distr <- zonal(ID.Raster, EmergingTrend_Reclass, sum)
  ID.Raster.Distr %<>% as.data.frame()
  a <- sum(ID.Raster.Distr$value)
  return(paste0("World region GDP(PPP) in trillions: ", round(a/1e12, 2)))
}

# Creating a function that takes the ID of the world region and returns the mean GDP per capita in region
GDP.pc.func <- function(WorldRegion_ID){
  ID.Raster <- raster(EmergingTrend)
  ID.Raster[] <- GDP[]
  ID.Raster[MAgPIE_ras != WorldRegion_ID] <- NA
  ID.Raster.Distr <- zonal(ID.Raster, EmergingTrend_Reclass, sum)
  ID.Raster.Distr %<>% as.data.frame()
  a <- sum(ID.Raster.Distr$value)
  b <- sum(Population[MAgPIE_ras == WorldRegion_ID], na.rm = TRUE)
  c <- a/b
  return(paste0("World region GDP(PPP) per capita in thousands: ", round(c/1e3, 2)))
}

# Creating a function that takes the ID of the world region and returns a density plot of the GDP distribution
# relative to emerging freshwater trends in the region
GDP.distr.plot <- function(WorldRegion_ID){
  ID.Raster <- raster(EmergingTrend)
  ID.Raster[] <- GDP[]
  ID.Raster[MAgPIE_ras != WorldRegion_ID] <- NA
  ID.Raster.Distr <- zonal(ID.Raster, EmergingTrend_Reclass, sum)
  ID.Raster.Distr %<>% as.data.frame()
  plot <- ggplot() +
  geom_density(data = ID.Raster.Distr, aes(x=zone, weight = value), fill = "grey", lwd = 0.8, adjust = 1/6) +
  scale_x_continuous(limits = c(370.5, 430.5), breaks = seq(370.5, 430.5, by = 10), expand = c(0,0),  
                     labels = c("-3", "-2", "-1", "0", "1", "2", "3")) +
  scale_y_continuous(expand = c(0,0))+
  geom_vline(xintercept = 400.5, lwd = 0.1) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_blank(), 
        axis.line = element_blank(), 
        legend.position = "none",
        text = element_blank(),
        line = element_blank())
  return(plot)
}

# MAgPIE world regions legend:
# 1 - North America 
# 2 - Former Soviet States
# 3 - Pacific OECD nations
# 4 - Pacific Asia
# 5 - Centrally Planned Asia
# 6 - South Asia
# 7 - Sub-Saharan Africa
# 8 - Middle East and North Africa
# 9 - Western Europe
# 10 - Latin America

# call any of the below functions 
GDP.func(1) # GDP in world region
GDP.pc.func(1) # GDP per capita in world region
GDP.distr.plot(1) # GDP density plot relative to emerging trends in freshwater availability
