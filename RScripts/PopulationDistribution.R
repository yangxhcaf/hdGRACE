library(ggplot2)
library(raster)
library(dplyr) 
library(rgdal) 
library(magrittr) 
library(sp) 
library(e1071) 
library(reshape2)

## Import raw emerging water availability trends (from Rodell et al. 2018)
# This requires the preprocessing of converting the provided .csv file to GeoTIFF
GRACE <- raster("E:/! Xander/! Research/GIS_files/GRACE/Rodell_raw.tif")

# Resample GRACE (emerging trends) to 0.05d resolution
s <- raster(GRACE) 
res(s) <- 0.05
EmergingTrend <- resample(GRACE, s, method='bilinear')
# Write raster for future use w/o needing to resample
writeRaster(EmergingTrend, "E:/! Xander/! Research/GIS_files/R_gis_exports/GRACE_0d05.tif")

# Load raw population raster (GWPv4, 2015)
Population_raw.2015 <- raster("E:/! Xander/! Research/GIS_files/PopulationRaster/2015/gpw-v4-population-count-rev11_2015_30_sec_tif/gpw_v4_population_count_rev11_2015_30_sec.tif")
# Resample population raster to 0.05d resolution
Pop.2015 <- raster::aggregate(Population_raw.2015, fact = 6, fun = sum)
# Write raster for future use w/o needing to resample
writeRaster(Pop.2015, "E:/! Xander/! Research/GIS_files/R_gis_exports/POP_2015_0d05_V1.tif")

# Load WRI Aqueduct raw data 
Aqueduct <- readOGR(dsn = "E:/! Xander/! Research/GIS_files/Aqueduct",
                    layer = "aqueduct_global_dl_20150409")
# simplify attribute table to only shape and water stress attributes
Aqueduct <- Aqueduct[,c(1:4,10:12)]

# add unique integer ID for each BWS category
Aqueduct$BWS_id <- ifelse(Aqueduct$BWS_cat == "1. Low (<10%)", 1,
                          ifelse(Aqueduct$BWS_cat == "2. Low to medium (10-20%)", 2,
                                 ifelse(Aqueduct$BWS_cat == "3. Medium to high (20-40%)", 3,
                                        ifelse(Aqueduct$BWS_cat == "4. High (40-80%)", 4,
                                               ifelse(Aqueduct$BWS_cat == "5. Extremely high (>80%)", 5,
                                                      ifelse(Aqueduct$BWS_cat == "Arid & low water use", 0, 99))))))

# write a new shapefile to use in converting vector file to raster at 0.05d resolution
writeOGR(Aqueduct, dsn="E:/! Xander/! Research/GIS_files/Aqueduct/WaterStress_WRI.shp", 
         layer = "WaterStress_WRI", driver="ESRI Shapefile", overwrite_layer=TRUE)
# import converted water stress raster (GIS specific software perform vector rasterization much faster than in R)
WaterStress <- raster("E:/! Xander/! Research/GIS_files/Aqueduct/WaterStress_WRI.tif")
# set all cells containing NA to set ID for no data
WaterStress[is.na(WaterStress[])] <- 99
WaterStress %<>% as.factor()

# split population raster by regions of water stress
# begin by initializing a raster for each class with same extent and resolution as rest of files
Pop.Low     <- raster(EmergingTrend) 
Pop.LowMed  <- raster(EmergingTrend)
Pop.MedHigh <- raster(EmergingTrend)
Pop.High    <- raster(EmergingTrend)
Pop.ExHigh  <- raster(EmergingTrend)
Pop.Arid    <- raster(EmergingTrend)
Pop.NoDat   <- raster(EmergingTrend)

# set initial population counts per cell to 0 for each class
Pop.Low[]     <- 0
Pop.LowMed[]  <- 0
Pop.MedHigh[] <- 0
Pop.High[]    <- 0
Pop.ExHigh[]  <- 0
Pop.Arid[]  <- 0
Pop.NoDat[]  <- 0

# populate population rasters per water stress class
Pop.Low[WaterStress == 1]     <- Pop.2015[WaterStress == 1]
Pop.LowMed[WaterStress == 2]  <- Pop.2015[WaterStress == 2]
Pop.MedHigh[WaterStress == 3] <- Pop.2015[WaterStress == 3]
Pop.High[WaterStress == 4]    <- Pop.2015[WaterStress == 4]
Pop.ExHigh[WaterStress == 5]  <- Pop.2015[WaterStress == 5]
Pop.Arid[WaterStress == 0]    <- Pop.2015[WaterStress == 0]
Pop.NoDat[WaterStress == 99]  <- Pop.2015[WaterStress == 99]

# Reclassify emerging trend into 0.1 cm/yr increment bins
ReclassRanges <- data.frame(low = seq(-40, 6.0, 0.1), high = seq(-39.9, 6.1, 0.1), ReCLASS = seq(1:461))
EmergingTrend_Reclass <- reclassify(EmergingTrend, ReclassRanges)

# Calculate population within each trend bin for each water stress class
Pop.Low.distr      <- zonal(Pop.Low, EmergingTrend_Reclass, sum)      %>% as.data.frame()
Pop.LowMed.distr   <- zonal(Pop.LowMed, EmergingTrend_Reclass, sum)   %>% as.data.frame()
Pop.MedHigh.distr  <- zonal(Pop.MedHigh, EmergingTrend_Reclass, sum)  %>% as.data.frame()
Pop.High.distr     <- zonal(Pop.High, EmergingTrend_Reclass, sum)     %>% as.data.frame()
Pop.ExHigh.distr   <- zonal(Pop.ExHigh, EmergingTrend_Reclass, sum)   %>% as.data.frame()
Pop.Arid.distr     <- zonal(Pop.Arid, EmergingTrend_Reclass, sum)     %>% as.data.frame()
Pop.NoDat.distr    <- zonal(Pop.NoDat, EmergingTrend_Reclass, sum)    %>% as.data.frame()

# merge all results, and melt dataframe for plotting
Pop.distr <- cbind(Pop.Low.distr, Pop.LowMed.distr, Pop.MedHigh.distr, Pop.High.distr,
                   Pop.ExHigh.distr, Pop.Arid.distr, Pop.NoDat.distr)
Pop.distr <- Pop.distr[,c(1,seq(2,14,2))]
colnames(Pop.distr) <- c("Trend", "Low", "LowMed", "MedHigh", "High", "ExHigh", "Arid", "NoDat")
Pop.distr.melted <- melt(Pop.distr, id.var = "Trend")


## plot results
# For reference: bin ID vs emerging trend:
# 390.5 = -1; 400.5 = 0; 410.5 = 1
figure <- ggplot(Pop.distr.melted, aes(x = Trend, y = value, fill = variable)) +
  geom_bar(stat = "identity", position="stack", colour = "black") +
  scale_fill_manual(values = c("#ffffa3", "#ffe600", "#ff9a00", "#ff1900", "#bb0007", "#808080", "#252525")) + 
  scale_y_continuous(position = "right", breaks = seq(0, 500e6, by = 50e6), expand = c(0,0), 
                     sec.axis = dup_axis(name = element_blank())) +
  scale_x_continuous(limits = c(350.5, 430.5), breaks = seq(350.5, 430.5, by = 10), expand = c(0,0),  
                     labels = c("-5" ,"-4", "-3", "-2", "-1", "0", "1", "2", "3")) +
  theme(panel.background = element_rect(fill = alpha("black", 0.03)),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "black", 0.3), linetype = "dashed", size = 0.5),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  geom_vline(xintercept = 400.5, size = 1.5, alpha = 0.8) +
  labs(x=expression(paste("Terrestrial Water Storage Rate of Change","  ","(cm", y^-1,")", sep="")),
       y = "Populaiton (in millions)") 
figure

## Skewness calculation
# create new dataframe 
SkewTable <- data.frame(Pop.distr[,1])
colnames(SkewTable) <- "Trend"
SkewTable$Pop <- Pop.distr$Low + Pop.distr$LowMed + Pop.distr$MedHigh + Pop.distr$High + Pop.distr$ExHigh + Pop.distr$Arid + Pop.distr$NoDat 
# need to simplify so that ~2 billion (2^31) vector data limit is not exceeded 
SkewTable$PopSimp <- as.integer(round(SkewTable$Pop/100, 0))
ExplodedSkewTable <- SkewTable[rep(rownames(SkewTable), SkewTable$PopSimp), ]
skewness(ExplodedSkewTable$Trend)
