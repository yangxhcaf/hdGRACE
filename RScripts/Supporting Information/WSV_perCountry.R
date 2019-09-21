library(dplyr)
library(rgdal)
library(raster)
library(magrittr)
library(ggplot2)

WSV.FLOOD <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/WSVI_FLOOD.tif")
WSV.STRESS <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/WSVI_STRESS.tif")
Pop.2015 <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/POP_2015_0d05.tif")

Country_ras <- raster("Z:/2.active_projects/Xander/! GIS_files/NaturalEarth/CountryID.tif")

Pop.X.WSV_Flood <- WSV.FLOOD * Pop.2015
Pop.X.WSV_Stress <- WSV.STRESS * Pop.2015

Nation_Flood.x.Pop <- zonal(Pop.X.WSV_Flood, Country_ras, sum)
Nation_Flood.x.Pop %<>% as.data.frame()
colnames(Nation_Flood.x.Pop) <- c("Country", "Flood.x.Pop")

Nation_Stress.x.Pop <- zonal(Pop.X.WSV_Stress, Country_ras, sum)
Nation_Stress.x.Pop %<>% as.data.frame()
colnames(Nation_Stress.x.Pop) <- c("Country", "Stress.x.Pop")

Nation_Pop <- zonal(Pop.2015, Country_ras, sum)
Nation_Pop %<>% as.data.frame()
colnames(Nation_Pop) <- c("Country", "Pop")


CountryScores <- cbind(Nation_Flood.x.Pop, Nation_Stress.x.Pop, Nation_Pop)
CountryScores <- CountryScores[,(-3)]
CountryScores <- CountryScores[,(-4)]
CountryScores <- CountryScores[,(-5)]

CountryScores$Pop.mu.FLOOD <- CountryScores$Flood.x.Pop / (CountryScores$Pop + 1)
CountryScores$Pop.mu.STRESS <- CountryScores$Stress.x.Pop / (CountryScores$Pop + 1)

Nation_Flood.WSVI %<>% as.data.frame()
colnames(Nation_Flood.WSVI) <- c("Country", "Flood_G")

Nation_Stress.WSVI <- zonal(Stress.WSVI, Country_ras, mean)
Nation_Stress.WSVI %<>% as.data.frame()
colnames(Nation_Stress.WSVI) <- c("Country", "Stress_G")

CountryScores <- cbind(Nation_Flood.current, Nation_Stress.current, Nation_Flood.WSVI, Nation_Stress.WSVI)
CountryScores[is.na(CountryScores)] <- 0