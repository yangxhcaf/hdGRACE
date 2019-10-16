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
GRACE <- raster("Z:/2.active_projects/Xander/! GIS_files/GRACE/Rodell_raw.tif")

# Resample GRACE (emerging trends) to 0.05d resolution
s <- raster(GRACE) 
res(s) <- 0.05
EmergingTrend <- resample(GRACE, s, method='bilinear')
# Write raster for future use w/o needing to resample
writeRaster(EmergingTrend, "Z:/2.active_projects/Xander/! GIS_files/GRACE/GRACE_coredata.tif", overwrite = TRUE)

# Load raw population raster (GWPv4, 2015)
Population_raw.2015 <- raster("Z:/2.active_projects/Xander/! GIS_files/PopulationRaster/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev10_2015_30_sec.tif")
# Resample population raster to 0.05d resolution
Pop.2015_UNWPP <- raster::aggregate(Population_raw.2015, fact = 6, fun = sum)
Pop.2015_UNWPP <- resample(Pop.2015_UNWPP, EmergingTrend, method = "ngb")
# Write raster for future use w/o needing to resample
writeRaster(Pop.2015_UNWPP, "Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/POP_2015_0d05_UNWPP.tif", overwrite = TRUE)

# Import converted Aqueduct (WRI, 2019) bws categorical raster (polygon file rasterized at 0.05d resolution in QGIS)
WaterStress <- raster("Z:/2.active_projects/Xander/! GIS_files/QGIS_exports/Aqueduct2019_bwscat.tif")
# set all cells containing NA to set ID for no data
WaterStress[is.na(WaterStress[])] <- -99
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
Pop.Low[WaterStress == 0]     <- Pop.2015_UNWPP[WaterStress == 0]
Pop.LowMed[WaterStress == 1]  <- Pop.2015_UNWPP[WaterStress == 1]
Pop.MedHigh[WaterStress == 2] <- Pop.2015_UNWPP[WaterStress == 2]
Pop.High[WaterStress == 3]    <- Pop.2015_UNWPP[WaterStress == 3]
Pop.ExHigh[WaterStress == 4]  <- Pop.2015_UNWPP[WaterStress == 4]
Pop.Arid[WaterStress == -1]    <- Pop.2015_UNWPP[WaterStress == -1]
Pop.NoDat[WaterStress == -99]  <- Pop.2015_UNWPP[WaterStress == -99]

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
figure.A <- ggplot(Pop.distr.melted, aes(x = Trend, y = value, fill = variable)) +
  geom_bar(stat = "identity", position="stack", colour = "black") +
  scale_fill_manual(values = c("#ffffa3", "#ffe600", "#ff9a00", "#ff1900", "#bb0007", "#808080", "#252525")) + 
  scale_y_continuous(position = "right", breaks = seq(0, 500e6, by = 50e6), expand = c(0,0), 
                     sec.axis = dup_axis(name = element_blank())) +
  scale_x_continuous(limits = c(350.5, 430.5), breaks = seq(350.5, 430.5, by = 10), expand = c(0,0),  
                     labels = c("-5" ,"-4", "-3", "-2", "-1", "0", "1", "2", "3")) +
  theme(panel.background =  element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "white", 0.3), linetype = "dashed", size = 0.5),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  geom_vline(xintercept = 400.5, size = 1.5, alpha = 0.8) +
  labs(x=expression(paste("Terrestrial Water Storage Rate of Change","  ","(cm", y^-1,")", sep="")),
       y = "Populaiton (in millions)") 
figure.A

ggsave("C:/Users/Tom/Desktop/PopulationDistr_UNWPP.png", figure.A, dpi = 500, width = 14, height = 10, bg = "transparent")

## Skewness calculation
# create new dataframe 
SkewTable <- data.frame(Pop.distr[,1])
colnames(SkewTable) <- "Trend"
SkewTable$Pop <- Pop.distr$Low + Pop.distr$LowMed + Pop.distr$MedHigh + Pop.distr$High + Pop.distr$ExHigh + Pop.distr$Arid + Pop.distr$NoDat 
# need to simplify so that ~2 billion (2^31) vector data limit is not exceeded 
SkewTable$PopSimp <- as.integer(round(SkewTable$Pop/100, 0))
ExplodedSkewTable <- SkewTable[rep(rownames(SkewTable), SkewTable$PopSimp), ]
skewness(ExplodedSkewTable$Trend)

sum(SkewTable$Pop)
Severe.Dry.pop <- SkewTable %>% filter(Trend < 380.5) %>% pull(Pop) %>% sum()
Mod.Dry.pop    <- SkewTable %>% filter(Trend > 380.5 & Trend < 395.5) %>% pull(Pop) %>% sum()
Neut.pop    <- SkewTable %>% filter(Trend > 395.5 & Trend < 405.5) %>% pull(Pop) %>% sum()
Mod.Wet.pop    <- SkewTable %>% filter(Trend > 405.5 & Trend < 420.5) %>% pull(Pop) %>% sum()
Sev.Wet.pop    <- SkewTable %>% filter(Trend > 420.5) %>% pull(Pop) %>% sum()


# code to plot mean TWS trend per water stress class

# Create raster of GRACE trend *times* population, for each water stress class
GRACE.bwsLOW <- (Pop.Low * EmergingTrend) %>% as.data.frame()
GRACE.bwsLOWMED <- Pop.LowMed * EmergingTrend %>% as.data.frame()
GRACE.bwsMEDHIGH <- Pop.MedHigh * EmergingTrend %>% as.data.frame()
GRACE.bwsHIGH <- Pop.High * EmergingTrend %>% as.data.frame()
GRACE.bwsEXHIGH <- Pop.ExHigh * EmergingTrend %>% as.data.frame()
GRACE.bwsARID <- Pop.Arid * EmergingTrend %>% as.data.frame()

Pop.Low %<>% as.data.frame() 
Pop.LowMed %<>% as.data.frame() 
Pop.MedHigh %<>% as.data.frame()  
Pop.High %<>% as.data.frame() 
Pop.ExHigh %<>% as.data.frame() 
Pop.Arid %<>% as.data.frame() 

MeanTWS_low <- sum(GRACE.bwsLOW, na.rm = TRUE) / sum(Pop.Low, na.rm = TRUE)
MeanTWS_lowmed <- sum(GRACE.bwsLOWMED, na.rm = TRUE) / sum(Pop.LowMed, na.rm = TRUE)
MeanTWS_medhigh <- sum(GRACE.bwsMEDHIGH, na.rm = TRUE) / sum(Pop.MedHigh, na.rm = TRUE)
MeanTWS_high <- sum(GRACE.bwsHIGH, na.rm = TRUE) / sum(Pop.High, na.rm = TRUE)
MeanTWS_exhigh <- sum(GRACE.bwsEXHIGH, na.rm = TRUE) / sum(Pop.ExHigh, na.rm = TRUE)
MeanTWS_arid <- sum(GRACE.bwsARID, na.rm = TRUE) / sum(Pop.Arid, na.rm = TRUE)

a <- rbind(MeanTWS_low, MeanTWS_lowmed, MeanTWS_medhigh, MeanTWS_high, MeanTWS_exhigh)
colnames(a) <- ("MeanTWS")
a %<>% as.data.frame()
a$Class <- seq(0, 4, by = 1)
a$Pop <- c(sum(Pop.Low, na.rm = TRUE), sum(Pop.LowMed, na.rm = TRUE),
           sum(Pop.MedHigh, na.rm = TRUE), sum(Pop.High, na.rm = TRUE),
           sum(Pop.ExHigh, na.rm = TRUE))

sum(a$Pop)

progress <- data.frame(Class = c(1,2,3,4,5), Muman = c(0.154,0.001,-0.045,-0.090,-0.833), Pop = c(2438, 888, 922, 1108, 1711))
progress

Arid <- data.frame(Class = 6, Muman = -0.299, Pop = 79)

figure.B <- ggplot() + 
  theme(panel.background =  element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "white", 0.3), linetype = "dashed", size = 0.5),
        axis.line = element_line(colour = "black"),
        axis.title = element_blank(),
        legend.position = "none") +
  geom_hline(yintercept = 0, size = 1.5) + 
  stat_smooth(data = a, aes(x = Class, y = MeanTWS), lwd = 2, colour = "black", method = "lm", formula = y ~ poly(x,4), se = FALSE) +
  geom_point(data = a, aes(x = Class, y = MeanTWS, size = Pop, fill = ..y..), pch = 21, stroke = 1.5) + 
  scale_fill_gradient2(low = "red 3", mid = "lemonchiffon2", high = "royalblue2",
                       midpoint = -0.15, guide = FALSE)+ 
  scale_radius("Population (millions)", range = c(6,15), trans = "sqrt") +  
  scale_x_reverse(labels = c("0" = "Low", "1" = "Low to Med.", "2" = "Med. to High", "3" = "High", "4" = "Extremely High")) +
  scale_y_continuous(limits = c(-1, 0.5), breaks = c(-1, -0.75, - 0.5, -0.25, 0, 0.25, 0.5), expand = c(0,0)) + 
  coord_flip()
figure.B

ggsave("C:/Users/Tom/Desktop/PopMeanTWS_UNWPP.png", figure.B, dpi = 500, width = 3.75, height = 12.2, bg = "transparent")
