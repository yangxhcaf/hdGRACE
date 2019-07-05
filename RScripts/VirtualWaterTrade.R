library(ggplot2)
library(dplyr)
library(magrittr)
library(rgdal)
library(raster)
library(sf)
library(RColorBrewer)

# import Virtual Water Trade (Water Footprint Network) table supplemented with country codes
VWT <- read.csv("E:/! Xander/! Research/GIS_files/VirtualWater/VirtualWater_CountryCodes.csv")

# import emerging freshwater availability trends 
EmergingTrend <- raster("E:/! Xander/! Research/GIS_files/R_gis_exports/GRACE_0d05.tif")

# import Country shapefile
Country.shp <- readOGR(dsn = "E:/! Xander/! Research/GIS_files/NaturalEarth",
                       layer = "ne_10m_admin_0_countries")
Country.shp <- Country.shp[,-(1:3)]
Country.shp <- Country.shp[,-(3:6)]
Country.shp <- Country.shp[,-(4)]
Country.shp <- Country.shp[,-(4:9)]
Country.shp <- Country.shp[,-(6:ncol(Country.shp))]
Country.shp <- Country.shp[,-(6:30)]
Country.shp <- Country.shp[,-(21:ncol(Country.shp))]
Country.shp <- Country.shp[,-(14:16)]
Country.shp <- Country.shp[order(Country.shp$ADM0_A3),]
# Create unique integer ID for each country
Country.shp$ID <- seq(1, nrow(Country.shp), 1)
# write to OGR and reimport as converted GeoTIFF
writeOGR(Country.shp, dsn="E:/! Xander/! Research/GIS_files/VirtualWater/CountryID.shp", 
         layer = "CountryID", driver="ESRI Shapefile", overwrite_layer=TRUE)
CountryID <- raster("E:/! Xander/! Research/GIS_files/VirtualWater/CountryID_v1.tif")

# calulate mean terrestrial water storage trends per nation
CountryGRACEtrend <- zonal(EmergingTrend, CountryID, "mean")
CountryGRACEtrend %<>% as.data.frame()
colnames(CountryGRACEtrend) <- c("ID", "TWS")
# Merge results with Country ID table
Country.GRACE <- merge.data.frame(Country.shp, CountryGRACEtrend, by.x = "ID", by.y = "ID", all = FALSE)
# combine with virtual water trade table
VWT.GRACE <- merge.data.frame(VWT, Country.GRACE, by.x = "Alpha3", by.y = "ADM0_A3", all = FALSE)
VWT.GRACE <- VWT.GRACE[, -(3:10)]
VWT.GRACE <- VWT.GRACE[, -(4:6)]

### For figure presentation, we provide an indicator per country based on corruption and wealth per capita, which is determined below
# import corruption scores
CPI_scores <- read.csv("C:/Users/Tom/Desktop/CPI_2018.csv")
Country.CPI <- merge.data.frame(Country.shp, CPI_scores, by.x = "ADM0_A3", by.y = "ISO3", all = FALSE)
Country.CPI <- Country.CPI[,-(3)]
Country.CPI <- Country.CPI[,-(4:5)]
# merge with main dataframe
VWT.GRACE.CPI <- merge.data.frame(VWT.GRACE, Country.CPI, by.x = "Alpha3", by.y = "ADM0_A3", all = FALSE)
VWT.GRACE.CPI <- VWT.GRACE.CPI[, -(6:7)]


## Total national wealth per capita
Wealth <- read.csv("E:/! Xander/! Research/GIS_files/Wealth/Wealth-AccountsData.csv")
Wealth %<>% filter(Indicator.Code == "NW.TOW.PC")
Wealth <- Wealth[-(1:17),]
Wealth <- Wealth[,-(3:7)]
Wealth <- Wealth[,-(5)]
Wealth.shp <- merge.data.frame(Country.shp, Wealth, by.x = "ADM0_A3", by.y = "Country.Code", all = FALSE)
Wealth.shp <- Wealth.shp[,-(2:6)]
# combine with main dataframe
VWT.GRACE.CPI.wealth <- merge.data.frame(VWT.GRACE.CPI, Wealth.shp, by.x = "Alpha3", by.y = "ADM0_A3", all = FALSE)

# Normalize and combine wealth and corruption datasets
VWT.GRACE.CPI.wealth$CPI <- VWT.GRACE.CPI.wealth$CPI.Score.2018/100
VWT.GRACE.CPI.wealth$Wealth <- VWT.GRACE.CPI.wealth$X2014/241224
VWT.GRACE.CPI.wealth$Wealth <- ifelse(VWT.GRACE.CPI.wealth$Wealth > 1, 1, VWT.GRACE.CPI.wealth$Wealth)
VWT.GRACE.CPI.wealth$Index <- (0.5 * VWT.GRACE.CPI.wealth$Wealth) + (0.5 * VWT.GRACE.CPI.wealth$CPI)

figure <- ggplot(data = VWT.GRACE.CPI.wealth, aes(x = TWS, y = VWT_cmyr)) +
  #theme_light() + 
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(colour = "white", linetype = "dashed", size = 0.5),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10, color = "black")) +
  geom_hline(yintercept = 0, size = 2) +
  geom_vline(xintercept = 0, size = 2) +
  geom_point(pch = 21, aes(fill = -Index), size = 5) +
  scale_fill_distiller(palette = "RdYlGn")+
  ylab(bquote('Net Annual Virtual Water Import '~(cmy^-1))) + 
  xlab(bquote('Terrestrial Water Storage Rate of Change'~(cmy^-1))) + 
  coord_cartesian(xlim = c(-1, 1), ylim = c(-10, 10)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous( expand = c(0, 0)) +
  geom_text(aes(label=NAME.x), hjust = 0.5, vjust = 0.5, size = 4)+
  # geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  geom_abline(intercept = 0, slope = -1, linetype = "dashed") 
figure
