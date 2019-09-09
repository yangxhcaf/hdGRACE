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
CountryID <- raster("Z:/2.active_projects/Xander/! GIS_files/VirtualWater/CountryID_v1.tif")

# Load raster that accounts for longitudinal width decreases with increasing latitude
AreaCORR <- raster("Z:/2.active_projects/Xander/! GIS_files/WGS84_longitudeCORR/LongitudeAreaCORR.tif")
AreaCORR_0d05 <- resample(AreaCORR, EmergingTrend, method = "ngb")

# Produce a TWS x AreaCORR raster
TWS_areaWeight <- EmergingTrend * AreaCORR_0d05

# calculate sum of Area weighted trend in each country
Country_TWSt_AreaWeighted <- zonal(TWS_areaWeight, CountryID, "sum")
Country_TWSt_AreaWeighted %<>% as.data.frame()
colnames(Country_TWSt_AreaWeighted) <- c("ID", "TWSt_AreaWt")
# Merge results with Country ID table
Country.GRACE <- merge.data.frame(Country.shp, Country_TWSt_AreaWeighted, by.x = "ID", by.y = "ID", all = FALSE)

# calculate sum of Area weightings in each country
Country_AreaCORR <- zonal(AreaCORR_0d05, CountryID, "sum")
Country_AreaCORR %<>% as.data.frame()
colnames(Country_AreaCORR) <- c("ID", "AreaWt")
# Merge results with Country ID table
Country.GRACE <- merge.data.frame(Country.GRACE, Country_AreaCORR, by.x = "ID", by.y = "ID", all = FALSE)

# Calculate area-weighted TWS trend in each country
Country.GRACE$TWS_TREND <- Country.GRACE$TWSt_AreaWt/Country.GRACE$AreaWt


# combine with virtual water trade table
VWT.GRACE <- merge.data.frame(VWT, Country.GRACE, by.x = "Alpha3", by.y = "ADM0_A3", all = FALSE)
keepcols_1 <- c("ID", "Alpha3", "Country", "VWT_cmyr", "NAME", "TWS_TREND")
VWT.GRACE <- VWT.GRACE[,keepcols_1, drop = FALSE]

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

# calulate mean indicator scores per nation

# Multiply indicator scores by longitudinal weights
VWT.ind_AreaWght <- VWT.ind*AreaCORR_0d05


# determine sum of area weighted index score in each country
VWT.Indicator <- zonal(VWT.ind_AreaWght, CountryID, "sum")
VWT.Indicator %<>% as.data.frame()
colnames(VWT.Indicator) <- c("ID", "Ind_AreaWght")

# Merge sum of area weightings in each country
VWT.Indicator <- merge.data.frame(Country.GRACE, VWT.Indicator, by.x = "ID", by.y = "ID", all = FALSE)
keepcols_2 <- c("ID", "ADM0_A3", "NAME", "Ind_AreaWght", "AreaWt")
VWT.Indicator <- VWT.Indicator[,keepcols_2, drop = FALSE]
VWT.Indicator$INDICATOR <- VWT.Indicator$Ind_AreaWght/VWT.Indicator$AreaWt

# Merge results with Country ID table
Country.GRACE_VWT_IND <- merge.data.frame(VWT.GRACE, VWT.Indicator, by.x = "ID", by.y = "ID", all = FALSE)

figure <- ggplot(data = Country.GRACE_VWT_IND, aes(x = TWS_TREND, y = VWT_cmyr)) +
  #theme_light() + 
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(colour = "white", linetype = "dashed", size = 0.5),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10, color = "black")) +
  geom_hline(yintercept = 0, size = 2) +
  geom_vline(xintercept = 0, size = 2) +
  geom_point(pch = 21, aes(fill = -INDICATOR), size = 5) +
  scale_fill_distiller(palette = "PRGn")+
  ylab(bquote('Net Annual Virtual Water Import '~(cmy^-1))) + 
  xlab(bquote('Terrestrial Water Storage Rate of Change'~(cmy^-1))) + 
  coord_cartesian(xlim = c(-3, 3), ylim = c(-8, 8)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous( expand = c(0, 0)) +
  # geom_text(aes(label=NAME.x), hjust = 0.5, vjust = 0.5, size = 2) +
  # geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  geom_abline(intercept = 0, slope = -1, linetype = "dashed") 
figure

ggsave("C:/Users/Tom/Desktop/VirtualWater_PrGn_wgt.png", figure, width = 14, height = 10, bg = "transparent")
