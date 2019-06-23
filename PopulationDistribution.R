# library(sp)
# library(raster)
# library(rgdal)
# library(dplyr)
# library(ggspatial)
# library(ggplot2)
# library(e1071)
# library(scales)

# Load GRACE DATA (at 0.5 d resolution)
GRACE <- raster("C:/Users/lab/Documents/! Xander/! Research/TWS/GRACE_upright.tif")
# GRACE_spdf <- as(GRACE, "SpatialPixelsDataFrame")
# GRACE_df <- as.data.frame(GRACE_spdf)
# colnames(GRACE_df) = c("value", "x", "y")
# head(GRACE_df)

# Resample GRACE to 0.05d resolution
s <- raster(nrow=3600, ncol=7200)
GRACE_0d05res <- resample(GRACE, s, method='bilinear')
# plot(GRACE_0d05res)

# Load population raster 
POPULATION2015 <- raster("C:/Users/lab/Documents/! Xander/! Research/GIS_files/PopulationRaster/2015/2015_population_0d05res.tif")
# plot(POPULATION2015)

# Reclassify GRACE to 0.2 increment bins
GRACE_reclassRanges <- data.frame(low = seq(-40, 6.0, 0.1), high = seq(-39.9, 6.1, 0.1), ReCLASS = seq(1:461))
# tail(GRACE_reclassRanges, 30)
GRACE_Reclass <- reclassify(GRACE_0d05res, GRACE_reclassRanges)
# plot(GRACE_0d2Reclass)

# Calculate zonal statistic (sum of population per each reclassified region based on TWS rate of change)
GlobalPopDistr <- zonal(POPULATION2015, GRACE_Reclass, sum)
GlobalPopDistr <-  as.data.frame(GlobalPopDistr)
# tail(GlobalPopDistr, 30)
# sum(GlobalPopDistr$value)

# For reference: ID vs TWS value:
#   350.5 : -5
#   360.5 : -4
#   370.5 : -3.0
#   380.5 : -2.0 
#   390.5 : -1.0
#   400.5 : 0
#   410.5 : +1
#   420.5 : +2
#   430.5 +3

# Calculating popuption within larger bands #  zone numbers need to be adjusted for 0.1 increments
# ResultsTable <- data.frame(Zone = c("<-2", "-2 < & < -1", "-1 < & < 1", "1 < & < 2", "2 < "), 
#                            Population = c(1,2,3,4,5))
# ResultsTable[1,2] <- GlobalPopDistr %>% filter(zone < 190.5) %>% summarize(Total = sum(value))
# ResultsTable[2,2] <- GlobalPopDistr %>% filter(zone >  190.5 & zone < 195.5) %>% summarize(Total = sum(value))
# ResultsTable[3,2] <- GlobalPopDistr %>% filter(zone >  195.5 & zone < 205.5) %>% summarize(Total = sum(value))
# ResultsTable[4,2] <- GlobalPopDistr %>% filter(zone >  205.5 & zone < 210.5) %>% summarize(Total = sum(value))   
# ResultsTable[5,2] <- GlobalPopDistr %>% filter(zone > 210.5) %>% summarize(Total = sum(value))
# ResultsTable$Millions <- ResultsTable$Population/1000000

# Calculating skewness (simplifying by rounding to the nearest million people)
SkewnessTable <- GlobalPopDistr
SkewnessTable$tenthousands <- SkewnessTable$value/10000
SkewnessTable$tenthousandsInt <- as.integer(round(SkewnessTable$tenthousands, 0))
rpsData <- SkewnessTable[rep(rownames(SkewnessTable), SkewnessTable$tenthousandsInt), ]
rpsData
skewness(rpsData$zone)

max(GlobalPopDistr$value)
# Plotting results

p1 <- ggplot(data = GlobalPopDistr, aes(x = zone, y = value/1000000, fill = zone))+
  geom_bar(stat = "identity", colour = "black") +
  scale_fill_gradientn(colours = c("red3", "red3","lemonchiffon2",
                                   "royalblue1", "royalblue4"),
                       values = rescale(c(350, 380.5, 403, 415.5, 431)),
                       space = "Lab")+
  scale_y_continuous(position = "right", breaks = seq(0, 500, by = 50), expand = c(0,0), 
                     sec.axis = dup_axis(name = element_blank())) +
  scale_x_continuous(limits = c(350.5, 430.5), breaks = seq(350.5, 430.5, by = 10), expand = c(0,0),  
                     labels = c("-5" ,"-4", "-3", "-2", "-1", "0", "1", "2", "3")) +
  theme(panel.background = element_rect(fill = alpha("black", 0.03)),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "black", 0.3), linetype = "dashed", size = 0.5),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  geom_vline(xintercept = 401.5, size = 1.5, alpha = 0.8) +
  labs(x=expression(paste("Terrestrial Water Storage Rate of Change","  ","(cm", y^-1,")", sep="")),
       y = "Populaiton (in millions)") 
p1

setwd("C:/Users/lab/Desktop")
ggsave("GlobalPopDistr_0d01.png", p1, dpi = 300, width = 18, height = 12.3)