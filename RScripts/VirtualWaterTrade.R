library(ggplot2)
library(dplyr)
library(magrittr)
library(rgdal)
library(raster)
library(sf)
library(RColorBrewer)

# import Virtual Water Trade (Water Footprint Network) table supplemented with country codes
VWT <- read.csv("E:/! GIS_files/VirtualWater/VirtualWater_CountryCodes.csv")

# import emerging freshwater availability trends 
EmergingTrend <- raster("E:/! GIS_files/R_gis_exports/GRACE_0d05.tif")

# import Country shapefile
Country.shp <- readOGR(dsn = "E:/! GIS_files/NaturalEarth",
                       layer = "ne_10m_admin_0_countries")
keepcol <- c("SOVEREIGNT", "ADM0_A3", "NAME", "NAME_LONG")
Country.shp <- Country.shp[,keepcol, drop = FALSE]
Country.shp <- Country.shp[order(Country.shp$ADM0_A3),]
# Create unique integer ID for each country
Country.shp$ID <- seq(1, nrow(Country.shp), 1)
# write to OGR and reimport as converted GeoTIFF
writeOGR(Country.shp, dsn="E:/! GIS_files/VirtualWater/CountryID.shp", 
         layer = "CountryID", driver="ESRI Shapefile", overwrite_layer=TRUE)
CountryID <- raster("E:/! GIS_files/VirtualWater/CountryID_v1.tif")

# calulate mean terrestrial water storage trends per nation
CountryGRACEtrend <- zonal(EmergingTrend, CountryID, "mean")
CountryGRACEtrend %<>% as.data.frame()
colnames(CountryGRACEtrend) <- c("ID", "TWS")
# Merge results with Country ID table
Country.GRACE <- merge.data.frame(Country.shp, CountryGRACEtrend, by.x = "ID", by.y = "ID", all = FALSE)
# combine with virtual water trade table
VWT.GRACE <- merge.data.frame(VWT, Country.GRACE, by.x = "Alpha3", by.y = "ADM0_A3", all = FALSE)
keepcol2 <- c("Alpha3", "Country", "VWT_cmyr", "NAME_LONG", "TWS")
VWT.GRACE <- VWT.GRACE[,keepcol2, drop = FALSE]

### For figure presentation, we provide an indicator per country based on corruption and wealth per capita, which is determined below
# import corruption scores
CPI_scores <- read.csv("E:/! GIS_files/Corruption/CPI_2018.csv")
Country.CPI <- merge.data.frame(Country.shp, CPI_scores, by.x = "ADM0_A3", by.y = "ISO3", all = FALSE)
keepcol3 <- c("ADM0_A3", "SOVEREIGNT", "NAME_LONG", "CPI.Score.2018")
Country.CPI <- Country.CPI[,keepcol3, drop = FALSE]

# merge with main dataframe
VWT.GRACE.CPI <- merge.data.frame(VWT.GRACE, Country.CPI, by.x = "Alpha3", by.y = "ADM0_A3", all = FALSE)
VWT.GRACE.CPI <- VWT.GRACE.CPI[, -(6:7)]

## Total national wealth per capita
Wealth <- read.csv("E:/! GIS_files/Wealth/Wealth-AccountsData.csv")
Wealth %<>% filter(Indicator.Code == "NW.TOW.PC")
keepcol4 <- c("ï..Country.Name" ,"Country.Code", "X2010", "X2014")
Wealth <- Wealth[, keepcol4, drop = FALSE]
Wealth <- Wealth[-(1:17),] # first 17 rows have regional (non-national) statistics
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
  coord_cartesian(xlim = c(-3, 3), ylim = c(-8, 8)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous( expand = c(0, 0)) +
  # geom_text(aes(label=NAME_LONG.x), hjust = 0.5, vjust = 0.5, size = 4)+
  # geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  geom_abline(intercept = 0, slope = -1, linetype = "dashed") 
figure
