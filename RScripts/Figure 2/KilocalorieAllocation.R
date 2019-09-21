library(ggplot2)
library(scales)
library(fontcm)
library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(ggspatial)
library(e1071)
library(lattice)
library(reshape2)
library(magrittr)

# Load GRACE DATA (at 0.5 d resolution)
GRACE <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/GRACE_0d05.tif")
FEED <- raster("Z:/2.active_projects/Xander/! GIS_files/CropAllocationFoodFeedFuel_Geotiff/GlbFeedkcal_Resample_0d05.tif")
FOOD <- raster("Z:/2.active_projects/Xander/! GIS_files/CropAllocationFoodFeedFuel_Geotiff/GlbFoodkcal_Resample_0d05.tif")
NONFOOD <- raster("Z:/2.active_projects/Xander/! GIS_files/CropAllocationFoodFeedFuel_Geotiff/GlbNonFoodkcal_Resample_0d05.tif")

# Reclassify GRACE to 0.1 increment bins
GRACE_reclassRanges <- data.frame(low = seq(-40, 6.0, 0.1), high = seq(-39.9, 6.1, 0.1), ReCLASS = seq(1:461))
GRACE_Reclass <- reclassify(GRACE, GRACE_reclassRanges)

# Calculate sum of kcal per each reclassified region based on TWS rate of change
FEED_Distr <- zonal(FEED, GRACE_Reclass, sum)
FEED_Distr <-  as.data.frame(FEED_Distr)
colnames(FEED_Distr)[colnames(FEED_Distr)=="value"] <- "FEED_kcal"
head(FEED_Distr, 3)
sum(FEED_Distr$FEED_kcal)

FOOD_Distr <- zonal(FOOD, GRACE_Reclass, sum)
FOOD_Distr <-  as.data.frame(FOOD_Distr)
colnames(FOOD_Distr)[colnames(FOOD_Distr)=="value"] <- "FOOD_kcal"
head(FOOD_Distr, 3)
sum(FOOD_Distr$FOOD_kcal)

NONFOOD_Distr <- zonal(NONFOOD, GRACE_Reclass, sum)
NONFOOD_Distr <-  as.data.frame(NONFOOD_Distr)
colnames(NONFOOD_Distr)[colnames(NONFOOD_Distr)=="value"] <- "NONFOOD_kcal"
head(NONFOOD_Distr, 3)
sum(NONFOOD_Distr$NONFOOD_kcal)

# FRAC_FEED <- sum(FEED_Distr$FEED_kcal) / (sum(FEED_Distr$FEED_kcal) + sum(FOOD_Distr$FOOD_kcal) + sum(NONFOOD_Distr$NONFOOD_kcal))
# FRAC_FOOD <- sum(FOOD_Distr$FOOD_kcal) / (sum(FEED_Distr$FEED_kcal) + sum(FOOD_Distr$FOOD_kcal) + sum(NONFOOD_Distr$NONFOOD_kcal))
# FRAC_NONFOOD <- sum(NONFOOD_Distr$NONFOOD_kcal) / (sum(FEED_Distr$FEED_kcal) + sum(FOOD_Distr$FOOD_kcal) + sum(NONFOOD_Distr$NONFOOD_kcal))
# print(paste0("Feed fraction: ", FRAC_FEED))
# print(paste0("Food fraction: ", FRAC_FOOD))
# print(paste0("NonFood fraction: ", FRAC_NONFOOD))


# Calculating skewness (simplifying by rounding to the nearest million people)
SkewnessFEED <- FEED_Distr
SkewnessFEED$billionKCAL <- SkewnessFEED$FEED_kcal/1000000000
SkewnessFEED$billionKCALINT <- as.integer(round(SkewnessFEED$billionKCAL, 0))
SkewnessFEED <- SkewnessFEED[rep(rownames(SkewnessFEED), SkewnessFEED$billionKCALINT), ]
skewness(SkewnessFEED$zone)
#
SkewnessFOOD <- FOOD_Distr
SkewnessFOOD$billionKCAL <- SkewnessFOOD$FOOD_kcal/1000000000
SkewnessFOOD$billionKCALINT <- as.integer(round(SkewnessFOOD$billionKCAL, 0))
SkewnessFOOD <- SkewnessFOOD[rep(rownames(SkewnessFOOD), SkewnessFOOD$billionKCALINT), ]
skewness(SkewnessFOOD$zone)
#
SkewnessNONFOOD <- NONFOOD_Distr
SkewnessNONFOOD$billionKCAL <- SkewnessNONFOOD$NONFOOD_kcal/1000000000
SkewnessNONFOOD$billionKCALINT <- as.integer(round(SkewnessNONFOOD$billionKCAL, 0))
SkewnessNONFOOD <- SkewnessNONFOOD[rep(rownames(SkewnessNONFOOD), SkewnessNONFOOD$billionKCALINT), ]
skewness(SkewnessNONFOOD$zone)
#
Globaldf <-KCAL_Global_types
Globaldf$total <- 1000*(Globaldf$FOOD_kcal + Globaldf$FEED_kcal + Globaldf$NONFOOD_kcal)
SkewnessGLOBAL <- Globaldf
SkewnessGLOBAL$totalINT <- as.integer(round(SkewnessGLOBAL$total, 0))
SkewnessGLOBAL <- SkewnessGLOBAL[rep(rownames(SkewnessGLOBAL), SkewnessGLOBAL$totalINT), ]
skewness(SkewnessGLOBAL$zone)

# Merge data frames
KCAL_Global_types <- merge.data.frame(FOOD_Distr, FEED_Distr, by.x = "zone", by.y = "zone")
KCAL_Global_types <- merge.data.frame(KCAL_Global_types, NONFOOD_Distr, by.x = "zone", by.y = "zone")
head(KCAL_Global_types, 15)

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

# convert kcal into trillions
KCAL_Global_types$FOOD_kcal <- KCAL_Global_types$FOOD_kcal / 1000000000000
KCAL_Global_types$FEED_kcal <- KCAL_Global_types$FEED_kcal / 1000000000000
KCAL_Global_types$NONFOOD_kcal <- KCAL_Global_types$NONFOOD_kcal / 1000000000000
keeps <- c("zone", "FOOD_kcal", "FEED_kcal", "NONFOOD_kcal")
KCAL_Global_types <- KCAL_Global_types[keeps]
head(KCAL_Global_types, 4)

# Plotting results
KCAL_Global_types_MELT <- melt(KCAL_Global_types, id.var = "zone")

# Find max value
# find max value 
max((KCAL_Global_types$FOOD_kcal) + (KCAL_Global_types$FEED_kcal) + (KCAL_Global_types$NONFOOD_kcal))

KCAL_Global_types_MELT$variable <- factor(KCAL_Global_types_MELT$variable)

p1 <- ggplot(KCAL_Global_types_MELT, aes(x = zone, y = value, fill = variable)) +
  geom_bar(stat = "identity", position="stack", colour = "black") +
  scale_fill_manual(values = c("palegreen3", "burlywood4", "gray32")) + 
  scale_y_continuous(position = "right", breaks = seq(0, 1750, by = 250), expand = c(0,0)) +
  scale_x_continuous(limits = c(350, 431), breaks = seq(350.5, 430.5, by = 10), expand = c(0,0),  
                     labels = c("-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size= 1.5),
        axis.title = element_text(size = 11, color = "black"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major.x = element_line(alpha(colour = "white"), linetype = "dashed", size = 0.5),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(alpha(colour = "white"), linetype = "dashed", size = 0.5),
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(face = "bold", size = 10, color = "black"),
        axis.title.y = element_text(color = "black"), 
        legend.position = "none") +
  geom_vline(xintercept = 400.5, size = 1.5) +
  labs(x=expression(paste("Terrestrial Water Storage Rate of Change","  ","(cm", y^-1,")", sep="")),
       y = "Global Food Production kcal (trillions)\n") 
p1

#
## Calculate percent of each distribution in severe wetting/drying conditions
#
head(KCAL_Global_types)
KCAL_Global_types$comb <- KCAL_Global_types$FOOD_kcal + KCAL_Global_types$FEED_kcal + KCAL_Global_types$NONFOOD_kcal

CombinedSevere <- KCAL_Global_types %>% filter(zone < 380.5 | zone > 420.5) %>% pull(comb) %>% sum()
CombinedSevere.pct <- CombinedSevere / sum(KCAL_Global_types$comb)

FoodSevere <- KCAL_Global_types %>% filter(zone < 380.5 | zone > 420.5) %>% pull(FOOD_kcal) %>% sum()
FoodSevere.pct <- CombinedSevere / sum(KCAL_Global_types$FOOD_kcal)

FoodSevereLoss <- KCAL_Global_types %>% filter(zone < 380.5) %>% pull(FOOD_kcal) %>% sum()
FoodSevereLoss.pct <- FoodSevereLoss / sum(KCAL_Global_types$FOOD_kcal)

FoodModerateLoss <- KCAL_Global_types %>% filter(zone < 395.5 & zone > 380.5) %>% pull(FOOD_kcal) %>% sum()
FoodModerateLoss.pct <- FoodModerateLoss / sum(KCAL_Global_types$FOOD_kcal)

FoodMinorLoss <- KCAL_Global_types %>% filter(zone > 395.5 & zone < 400.5) %>% pull(FOOD_kcal) %>% sum()
FoodMinorLoss.pct <- FoodMinorLoss / sum(KCAL_Global_types$FOOD_kcal)

FeedSevereLoss          <- KCAL_Global_types %>% filter(zone < 380.5 ) %>% pull(FEED_kcal) %>% sum()
NonfoodSevereLoss       <- KCAL_Global_types %>% filter(zone < 380.5 ) %>% pull(NONFOOD_kcal) %>% sum()
NonHumanSevereLoss      <- FeedSevereLoss + NonfoodSevereLoss
NonHumanSevereLoss.pct  <- NonHumanSevereLoss / (sum(KCAL_Global_types$FEED_kcal) + sum(KCAL_Global_types$NONFOOD_kcal))


FeedSevereLoss <- KCAL_Global_types %>% filter(zone < 380.5) %>% pull(FEED_kcal) %>% sum()
NFSevereLoss <- KCAL_Global_types %>% filter(zone < 380.5) %>% pull(NONFOOD_kcal) %>% sum()
FeedModerateLoss <- KCAL_Global_types %>% filter(zone < 395.5 & zone > 380.5) %>% pull(FEED_kcal) %>% sum()
NFModerateLoss <- KCAL_Global_types %>% filter(zone < 395.5 & zone > 380.5) %>% pull(NONFOOD_kcal) %>% sum()
FeedMinorLoss <- KCAL_Global_types %>% filter(zone > 395.5 & zone < 400.5) %>% pull(FEED_kcal) %>% sum()
NFMinorLoss <- KCAL_Global_types %>% filter(zone > 395.5 & zone < 400.5) %>% pull(NONFOOD_kcal) %>% sum()

SevereLoss.pct <- (FeedSevereLoss+NFSevereLoss) / (sum(KCAL_Global_types$FEED_kcal)+sum(KCAL_Global_types$NONFOOD_kcal))
ModerateLoss.pct <- (FeedModerateLoss+NFModerateLoss) / (sum(KCAL_Global_types$FEED_kcal)+sum(KCAL_Global_types$NONFOOD_kcal))
MinorLoss.pct <- (FeedMinorLoss+NFMinorLoss) / (sum(KCAL_Global_types$FEED_kcal)+sum(KCAL_Global_types$NONFOOD_kcal))

FoodSevereGain <- KCAL_Global_types %>% filter(zone > 420.5) %>% pull(FOOD_kcal) %>% sum()
FoodSevereGain.pct <- FoodSevereGain / sum(KCAL_Global_types$FOOD_kcal)

FeedLoss          <- KCAL_Global_types %>% filter(zone < 400.5 ) %>% pull(FEED_kcal) %>% sum()
NonfoodLoss       <- KCAL_Global_types %>% filter(zone < 400.5 ) %>% pull(NONFOOD_kcal) %>% sum()
NonHumanLoss      <- FeedLoss + NonfoodLoss
NonHumanLoss.pct  <- NonHumanLoss / (sum(KCAL_Global_types$FEED_kcal) + sum(KCAL_Global_types$NONFOOD_kcal))

#
## Calculate total kilocalorie production in each cropland intensity bin
#
Kilocal.production <- overlay(FEED, FOOD, NONFOOD, fun=function(x,y,z){return(x+y+z)})
plot(Kilocal.production)

Cropland.intensity <- raster("E:/! Xander/! Research/GIS_files/Cropland/Crop_bins0d5.tif")
Cropland.intensity <- resample(Cropland.intensity, Kilocal.production, method = "ngb")  # just to make sure origin of rasters is the same
plot(Cropland.intensity)

Kcal.per.CropIntensitybin <- zonal(Kilocal.production, Cropland.intensity, fun = "sum", na.rm = TRUE)
Kcal.per.CropIntensitybin <-  as.data.frame(Kcal.per.CropIntensitybin)
colnames(Kcal.per.CropIntensitybin) <- c("i", "kcal")
a <- Kcal.per.CropIntensitybin %>% pull(kcal) %>% sum()
Kcal.per.CropIntensitybin$pct <- round(100 * Kcal.per.CropIntensitybin$kcal / a, 2)
head(Kcal.per.CropIntensitybin, 20)

Kcal.per.CropIntensitybin %>% filter(i < 12 ) %>% pull(pct) %>% sum()

Cropland.intensity_df <- Cropland.intensity %>% as.data.frame()
Cropland.intensity_df$Crop_bins0d5 <- as.factor(Cropland.intensity_df$Crop_bins0d5)

summary(Cropland.intensity_df, na.rm = TRUE, 22)

zz <- ggplot(Kcal.per.CropIntensitybin, aes(x=i, y=pct)) +
  geom_area(aes(), fill = "black") +
  theme(axis.title = element_text(size = 11, color = "black"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        # axis.line = element_blank(),
        # axis.text = element_blank(),
        # axis.title.y = element_blank(),
        # axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none") +
  scale_x_continuous(position = "right", breaks = seq(0, 20, by = 1), expand = c(0,0)) +
  geom_line() + geom_point(alpha = 0) + 
  coord_flip(xlim = c(1,20)) 


setwd("C:/Users/Tom/Desktop")
ggsave("kcal_production.png", zz, dpi = 500, width = 5, height = 10, bg = "transparent")

