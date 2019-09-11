library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(magrittr)
library(scales)
library(reshape2)
library(RColorBrewer)
library(Hmisc)
library(quantreg)
library(spatstat)
library(reldist)

# Import Rodell et al. (2018) emerging freshwater availability trend as done in previous scripts
EmergingTrend <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/GRACE_0d05.tif")

# Import Ramankutty et al. (2008) raw dataset of cropland density
CroplandDensity <- raster("E:/! GIS_files/Cropland/cropland.tif")
# and resample to 0.05d resolution
e <- extent(-180, 180, -90, 90)
extent(CroplandDensity) <- e
CroplandDensity_resample <- resample(CroplandDensity, EmergingTrend, method = "bilinear")
# write raster so resampling isn't required to rerun the code
writeRaster(CroplandDensity_resample, "E:/! GIS_files/R_gis_exports/cropland.tif")

# Can start here if resampling already done
CroplandDensity <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/CroplandDensity_0d05.tif")

# Reclassify cropland density into 0.05 bins
ReclassRanges <- data.frame(low = seq(0, 0.95, 0.05), high = seq(0.05, 1.00, 0.05), ReCLASS = seq(1:20))
CroplandDensity_binned <- reclassify(CroplandDensity, ReclassRanges)

# load raster cell area in km2 at 0d05 resolution
CellArea <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/WGS84_cellArea_0d05res.tif")
CellArea_norm <- CellArea/max(CellArea[])


# Merge emerging trend raster with cropland density raster
EmergingTrend_df <- EmergingTrend %>% as.data.frame()
CroplandDensity_binned_df <- CroplandDensity_binned %>% as.data.frame()
CellArea_norm_df <- CellArea_norm %>% as.data.frame()

CropDensity.TWSt.AreaWgt <- cbind(EmergingTrend_df, CroplandDensity_binned_df, CellArea_norm_df)
CropDensity.TWSt.AreaWgt$CroplandDensity_0d05 %<>% as.factor()
CropDensity.TWSt.AreaWgt <- CropDensity.TWSt.AreaWgt[complete.cases(CropDensity.TWSt.AreaWgt), ]
colnames(CropDensity.TWSt.AreaWgt) <- c("GRACE", "cropland", "AreaWgt")

#colour brewer
coul = brewer.pal(5, "YlGn") 
coul = colorRampPalette(coul)(19)

# determine distribution statistics for the coupled raster grids
summ <- CropDensity.TWSt.AreaWgt %>% 
  group_by(cropland) %>%
  summarise(WeightedMean = weighted.mean(GRACE, AreaWgt),
            WeightedMedian = weighted.median(GRACE, AreaWgt),
            Weightedp25 = weighted.quantile(GRACE, AreaWgt, probs = 0.25),
            Weightedp75 = weighted.quantile(GRACE, AreaWgt, probs = 0.75),
            WeightedpLOW = weighted.quantile(GRACE, AreaWgt, probs = 0.05),
            WeightedHIGH = weighted.quantile(GRACE, AreaWgt, probs = 0.95))
summ$WeightedIQR <- summ$Weightedp75 - summ$Weightedp25

# format results for ggplot boxplot figure generation 
summDF <- data.frame(x= summ$cropland, min=summ$WeightedpLOW, 
                     low=summ$Weightedp25, wgt.mean = summ$WeightedMean, mid = summ$WeightedMedian, 
                     top=summ$Weightedp75, max= summ$WeightedHIGH)
summDF$x %<>% as.factor()
summDF <- summDF %>% filter(x != "0" &  x != "1") # remove bare areas from plot

### below is box plot with area weightings applied
Figure2b <- ggplot(summDF, aes(x = x, ymin = min, lower = low, middle = mid, upper = top, ymax = max, fill = x)) +
  geom_hline(yintercept = 0, size = 1.5) +
  geom_hline(yintercept = -3, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = -2, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = -1, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 1, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 2, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 3, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_boxplot(width = 0.75, stat = "identity", outlier.alpha = 0, lwd = 0.75) +
  geom_path(group = "1", position = "identity", stat = "identity", aes(x = x, y = wgt.mean), color = "red2", size = 1.6) +
  scale_x_discrete(limits=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")) +
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
  scale_fill_manual(values = coul) +
  coord_flip(ylim = c(-3, 3))+
  xlab("cropland") + ylab("GRACE") + labs(fill = "cropland")
Figure2b

ggsave("C:/Users/Tom/Desktop/CellArea_figures/CroplandDensity_AreaWgt.png", Figure2b, 
       dpi = 500, width = 7, height = 7, bg = "transparent")

### Below is some code to determine the statistical significance differentiating distributions per cropland density
ttestFunction <- function(bin1){
  BinA <- CropDensity.TWSt.AreaWgt %>% filter(cropland == bin1) %>% pull(GRACE)
  BinB <- CropDensity.TWSt.AreaWgt %>% filter(cropland == bin1-1) %>% pull(GRACE) 
  result <- t.test(BinA, BinB, "two.sided", equal.var = FALSE)
  return(result)
}

# run ttest for all adjacent bins
UpperBin <- c(seq(20, 3, by = -1)) %>% as.list()
lapply(UpperBin, ttestFunction)
