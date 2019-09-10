library(ggplot2)
library(dplyr)
library(magrittr)
library(rgdal)
library(raster)
library(sf)
library(RColorBrewer)

# import Virtual Water Trade (Water Footprint Network) table supplemented with country codes
VWT <- read.csv("Z:/2.active_projects/Xander/! GIS_files/VirtualWater/VirtualWater_CountryCodes.csv")

# import emerging freshwater availability trends 
EmergingTrend <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/GRACE_0d05.tif")

# import Country shapefile (add associated attribute table)
Country.shp <- readOGR(dsn = "Z:/2.active_projects/Xander/! GIS_files/NaturalEarth",
                       layer = "ne_10m_admin_0_countries")
keepcols <- c("SOVEREIGNT", "SOV_A3", "ADM0_A3", "NAME")
Country.shp <- Country.shp[,keepcols, drop = FALSE]
Country.shp <- Country.shp[order(Country.shp$ADM0_A3),]
# Create unique integer ID for each country
Country.shp$ID <- seq(1, nrow(Country.shp), 1)

# import country extents IDed to correspond to attribute table
CountryID <- raster("Z:/2.active_projects/Xander/! GIS_files/VirtualWater/CountryID_v1.tif")

# load raster cell area in km2 at 0d05 resolution
CellArea <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/WGS84_cellArea_0d05res.tif")
CellArea_norm <- CellArea/max(CellArea[])

# Assess sum of area-weighted GRACE trend per country
Country_GRACE.cellsize <- zonal(EmergingTrend*CellArea_norm, CountryID, "sum")
Country_GRACE.cellsize %<>% as.data.frame()
colnames(Country_GRACE.cellsize) <- c("ID", "TWSt_AreaWt")
# Merge results with Country ID table
Country.shp <- merge.data.frame(Country.shp, Country_GRACE.cellsize, by.x = "ID", by.y = "ID", all = FALSE)

# Assess sum of normalized cell area per country
Country_size <- zonal(CellArea_norm, CountryID, "sum")
Country_size %<>% as.data.frame()
colnames(Country_size) <- c("ID", "AreaWt")
# Merge results with Country ID table
Country.shp <- merge.data.frame(Country.shp, Country_size, by.x = "ID", by.y = "ID", all = FALSE)

# Calculate area-weighted TWS trend in each country
Country.shp$TWS_TREND <- Country.shp$TWSt_AreaWt/Country.shp$AreaWt

# combine with virtual water trade table
VWT.GRACE <- merge.data.frame(VWT, Country.shp, by.x = "Alpha3", by.y = "ADM0_A3", all = FALSE)
keepcols_1 <- c("ID", "Alpha3", "Country", "VWT_cmyr", "NAME", "TWS_TREND")
VWT.GRACE <- VWT.GRACE[,keepcols_1, drop = FALSE] # ready to plot now

### For figure presentation, we provide an indicator per country based on corruption and wealth per capita, which is determined below
# import corruption scores
Corruption <- raster("Z:/2.active_projects/Xander/! GIS_files/Corruption/CPI_0d05.tif")
GNIpc_ras <- raster("Z:/2.active_projects/Xander/! GIS_files/Wealth/GNIpc_0d05.tif")

# employ same methods used in vulnerability filter code
Corruption.ind <- Corruption/100
Wealth.ind <- GNIpc_ras/12055
Wealth.ind[Wealth.ind > 1] <- 1
## set wealth.ind to corr.ind in instances where national wealth is not reported
Wealth.ind[is.na(Wealth.ind)] <- Corruption.ind[is.na(Wealth.ind)]
VWT.ind <- (1/2)*(Corruption.ind + Wealth.ind)

# Don't need to area weight indicator results, because both are provided at the national scale
# determine sum of area weighted index score in each country
VWT.Indicator <- zonal(VWT.ind, CountryID, "median")
VWT.Indicator %<>% as.data.frame()
colnames(VWT.Indicator) <- c("ID", "Indicator")

# Merge sum of area weightings in each country
VWT.Indicator <- merge.data.frame(Country.shp, VWT.Indicator, by.x = "ID", by.y = "ID", all = FALSE)
keepcols_2 <- c("ID", "ADM0_A3", "NAME", "Indicator")
VWT.Indicator <- VWT.Indicator[,keepcols_2, drop = FALSE]

# Merge results with Country ID table
VirtualWater_table <- merge.data.frame(VWT.GRACE, VWT.Indicator, by.x = "ID", by.y = "ID", all = FALSE)

Figure4a <- ggplot(data = VirtualWater_table, aes(x = TWS_TREND, y = VWT_cmyr)) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(colour = "white", linetype = "dashed", size = 0.5),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10, color = "black")) +
  geom_hline(yintercept = 0, size = 2) +
  geom_vline(xintercept = 0, size = 2) +
  geom_point(pch = 21, aes(fill = -Indicator), size = 5) +
  scale_fill_distiller(palette = "PRGn")+
  ylab(bquote('Net Annual Virtual Water Import '~(cmy^-1))) + 
  xlab(bquote('Terrestrial Water Storage Rate of Change'~(cmy^-1))) + 
  coord_cartesian(xlim = c(-3, 3), ylim = c(-8, 8)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous( expand = c(0, 0)) +
  # geom_text(aes(label=NAME.x), hjust = 0.5, vjust = 0.5, size = 2) + # use if want to see auto generated labels
  geom_abline(intercept = 0, slope = -1, linetype = "dashed") 
Figure4a

ggsave("C:/Users/Tom/Desktop/CellArea_figures/VirtualWater_areaweighted.png", Figure4a, 
       dpi = 500, width = 14, height = 10, bg = "transparent")




### Below is code to take country shapefiles and convert to raster with country IDs

# import Country shapefile
Country.shp <- readOGR(dsn = "Z:/2.active_projects/Xander/! GIS_files/NaturalEarth",
                       layer = "ne_10m_admin_0_countries")
keepcols <- c("SOVEREIGNT", "SOV_A3", "ADM0_A3", "NAME")
Country.shp <- Country.shp[,keepcols, drop = FALSE]
Country.shp <- Country.shp[order(Country.shp$ADM0_A3),]
# Create unique integer ID for each country
Country.shp$ID <- seq(1, nrow(Country.shp), 1)
# write to OGR and reimport as converted GeoTIFF
writeOGR(Country.shp, dsn="Z:/2.active_projects/Xander/! GIS_files/VirtualWater/CountryID.shp", 
         layer = "CountryID", driver="ESRI Shapefile", overwrite_layer=TRUE)
