library(raster)
library(rgdal)
library(dplyr)
library(ggspatial)
library(ggplot2)
library(e1071)
library(scales)
library(spatstat)
library(reldist)
library(magrittr)
library(reshape2)
library(Hmisc)
library(quantreg)
library(spatstat)
library(reldist)

### Load Data

# Load GRACE DATA (at 0.5 d resolution)
EmergingTrend <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/GRACE_0d05.tif")

# Load amphibian species richness dataset
Amphibians <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/AmphSpRch__0d05.tif")

# load raster cell area in km2 at 0d05 resolution
CellArea <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/WGS84_cellArea_0d05res.tif")
CellArea_norm <- CellArea/max(CellArea[])

# Merge GRACE data with RAW species richness dataset (at 0d05) with area weightings
EmergingTrend_df <- EmergingTrend_df %>% as.data.frame()
Amphibians_df <- Amphibians %>% as.data.frame()
CellArea_norm_df <- CellArea_norm %>% as.data.frame()
Biodiversity_analysis <- cbind(GRACE_df, Amphibians_df, CellArea_norm_df)
colnames(Biodiversity_analysis) <- c("GRACE", "Spc.Rich", "AreaWgt")
Biodiversity_analysis$Spc.Rich <- ifelse(Biodiversity_analysis$Spc.Rich < 1, NA, Biodiversity_analysis$Spc.Rich)
Biodiversity_analysis <- Biodiversity_analysis[complete.cases(Biodiversity_analysis), ]

P10 <- Biodiversity_analysis %>% summarise(x = weighted.quantile(Spc.Rich, AreaWgt, probs = 0.1)) %>% unname() %>% as.numeric()
P20 <- Biodiversity_analysis %>% summarise(x = weighted.quantile(Spc.Rich, AreaWgt, probs = 0.2)) %>% unname() %>% as.numeric() 
P30 <- Biodiversity_analysis %>% summarise(x = weighted.quantile(Spc.Rich, AreaWgt, probs = 0.3)) %>% unname() %>% as.numeric()
P40 <- Biodiversity_analysis %>% summarise(x = weighted.quantile(Spc.Rich, AreaWgt, probs = 0.4)) %>% unname() %>% as.numeric()
P50 <- Biodiversity_analysis %>% summarise(x = weighted.quantile(Spc.Rich, AreaWgt, probs = 0.5)) %>% unname() %>% as.numeric()
P60 <- Biodiversity_analysis %>% summarise(x = weighted.quantile(Spc.Rich, AreaWgt, probs = 0.6)) %>% unname() %>% as.numeric()
P70 <- Biodiversity_analysis %>% summarise(x = weighted.quantile(Spc.Rich, AreaWgt, probs = 0.7)) %>% unname() %>% as.numeric()
P80 <- Biodiversity_analysis %>% summarise(x = weighted.quantile(Spc.Rich, AreaWgt, probs = 0.8)) %>% unname() %>% as.numeric()
P90 <- Biodiversity_analysis %>% summarise(x = weighted.quantile(Spc.Rich, AreaWgt, probs = 0.9)) %>% unname() %>% as.numeric()
P100 <- Biodiversity_analysis %>% summarise(x = weighted.quantile(Spc.Rich, AreaWgt, probs = 1.0)) %>% unname() %>% as.numeric()

# reclassify/bin species richness distribution based on percentiles calculated
Low <- c(0, P10, P20, P30, P40, P50, P60, P70, P80, P90)
High <- c(P10, P20, P30, P40, P50, P60, P70, P80, P90, P100)
Reclss <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

ReclssTable <- cbind(Low, High, Reclss) 
names(ReclssTable) <- NULL

AmphSpRch_Deciles <- raster::reclassify(Amphibians,ReclssTable) 

# Merge GRACE data with binned species richness with area weights
EmergingTrend_df <- EmergingTrend_df %>% as.data.frame()
AmphSpRch_Deciles_df <- AmphSpRch_Deciles %>% as.data.frame()
CellArea_norm_df <- CellArea_norm %>% as.data.frame()

Biodiversity_analysis <- cbind(EmergingTrend_df, AmphSpRch_Deciles_df, CellArea_norm_df)
colnames(Biodiversity_analysis) <- c("GRACE", "SpsRch_bin", "AreaWgt")
Biodiversity_analysis$SpsRch_bin %<>% as.factor()
Biodiversity_analysis <- Biodiversity_analysis[complete.cases(Biodiversity_analysis), ]

# Calculate area weighted TWSt stats
summ <- Biodiversity_analysis %>% 
  group_by(SpsRch_bin) %>%
  summarise(WeightedMean = weighted.mean(GRACE, AreaWgt),
            WeightedMedian = weighted.median(GRACE, AreaWgt),
            Weightedp25 = weighted.quantile(GRACE, AreaWgt, probs = 0.25),
            Weightedp75 = weighted.quantile(GRACE, AreaWgt, probs = 0.75))
summ$WeightedIQR <- summ$Weightedp75 - summ$Weightedp25

summDF <- data.frame(x= summ$SpsRch_bin, min=summ$Weightedp25 - 1.5*summ$WeightedIQR, 
                     low=summ$Weightedp25, mid = summ$WeightedMedian, 
                     top=summ$Weightedp75, max= summ$Weightedp75 + 1.5*summ$WeightedIQR)
summDF$x %<>% as.factor()
summDF <- summDF %>% filter(x != "0") # remove regions of NA biodiversity

## below is area weighted box plot
Figure2e <-  ggplot(summDF, aes(x = x, ymin = min, lower = low, middle = mid, upper = top, ymax = max, fill = x)) +
  geom_hline(yintercept = 0, size = 1.5) +
  geom_hline(yintercept = -3, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = -2, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = -1, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 1, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 2, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 3, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_boxplot(width = 0.75, lwd = 0.7, stat = "identity", outlier.alpha = 0, fill = "#A4A4A4") +
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  scale_y_continuous(breaks = seq(-3, 3, by = 1), expand = c(0,0)) +
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
        axis.title.y = element_text(color = "black")) +
  # scale_fill_manual(values = coul) +
  coord_flip(ylim = c(-2, 2))
Figure2e 

ggsave("C:/Users/Tom/Desktop/CellArea_figures/Biodiversity_AreaWeighted.png", Figure2e, 
       dpi = 500, width = 6, height = 7, bg = "transparent")

### Code to take RAW amphibian species richness dataset and aggregate to 0.05 degree resolution
#
#
AmphSpRch <- raster("Z:/2.active_projects/Xander/! GIS_files/Biodiversity/SEDAC/all_amphibians.tif")
# resample to 0.05d resolution
AmphSpRch_0d05res <- raster::aggregate(AmphSpRch, fact = 6, fun = mean, expand = FALSE, na.rm = FALSE)
# need to resample using nearest neighbour because resolution and extent don't completely align
AmphSpRch_product <- resample(AmphSpRch_0d05res, GRACE_0d05res, method = "ngb")
# Load land mass raster (using countries)
CountryID <- raster("Z:/2.active_projects/Xander/! GIS_files/VirtualWater/CountryID_v1.tif") 
# Min. = 1; tf ocean is where countryID < 1

# set AmphSpRch_product to NA where ocean is
AmphSpRch_product[CountryID < 1] <- NA
AmphSpRch_product[is.na(CountryID)] <- NA

writeRaster(AmphSpRch_product, 
            filename="Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/AmphSpRch__0d05.tif", 
            format="GTiff", overwrite=TRUE)
