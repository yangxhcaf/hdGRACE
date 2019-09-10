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

# Load land use raster
LandUse <- raster("Z:/2.active_projects/Xander/! GIS_files/LandUse/GlobalLandUse_0d05.tif") 

# create reclassification matrix to generalize land use to IPCC classes
old <- c(10, 11, 12, 20, 30, 40, 
         50, 60, 61, 62, 70, 71, 72, 80, 81, 82, 90, 100, 160, 170, 
         110, 130,
         180,
         190,
         120, 121, 122, 140, 150, 151, 152, 153,
         200, 201, 202,
         210)
new <- c(1, 1, 1, 1, 1, 1, # agriculture
         2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, # forest
         3, 3, # grassland
         4, # wetland
         5, # urban
         6, 6, 6, 6, 6, 6, 6, 6, # shrubland/vegetation
         7, 7, 7, # bare area
         8) # water
Substitute_df <- data.frame(old, new)
LandUse_CategoryBin <- subs(LandUse, Substitute_df, 1, 2, subsWithNA=TRUE)

# load raster cell area in km2 at 0d05 resolution
CellArea <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/WGS84_cellArea_0d05res.tif")
CellArea_norm <- CellArea/max(CellArea[])

# Merge GRACE data with binned species richness
GRACE_df <- GRACE_0d05 %>% as.data.frame()
LandUseBin_df <- LandUse_CategoryBin %>% as.data.frame()
CellArea_norm_df <- CellArea_norm %>% as.data.frame()
GRACE_LandUseBin_AreaWgt_df <- cbind(GRACE_df, LandUseBin_df, CellArea_norm_df)
colnames(GRACE_LandUseBin_AreaWgt_df) <- c("GRACE", "LandUseCat", "AreaWgt")
GRACE_LandUseBin_AreaWgt_df <- GRACE_LandUseBin_AreaWgt_df[complete.cases(GRACE_LandUseBin_AreaWgt_df), ]
GRACE_LandUseBin_AreaWgt_df$LandUseCat <- as.factor(GRACE_LandUseBin_AreaWgt_df$LandUseCat)

# determine distribution statistics for the coupled raster grids
summ <- GRACE_LandUseBin_AreaWgt_df %>% 
  group_by(LandUseCat) %>%
  summarise(WeightedMean = weighted.mean(GRACE, AreaWgt),
            WeightedMedian = weighted.median(GRACE, AreaWgt),
            Weightedp25 = weighted.quantile(GRACE, AreaWgt, probs = 0.25),
            Weightedp75 = weighted.quantile(GRACE, AreaWgt, probs = 0.75))
summ$WeightedIQR <- summ$Weightedp75 - summ$Weightedp25

# format results for ggplot boxplot figure generation 
summDF <- data.frame(x= summ$LandUseCat, min=summ$Weightedp25 - 1.5*summ$WeightedIQR, 
                     low=summ$Weightedp25, wgt.mean = summ$WeightedMean, mid = summ$WeightedMedian, 
                     top=summ$Weightedp75, max= summ$Weightedp75 + 1.5*summ$WeightedIQR)
summDF$x %<>% as.factor()
summDF <- summDF %>% filter(x != "8") # remove want land use from plot

# plot figure
Figure2d <- ggplot(summDF, aes(x = x, ymin = min, lower = low, middle = mid, upper = top, ymax = max, fill = x)) +
  geom_hline(yintercept = 0, size = 1.5) +
  geom_hline(yintercept = -3, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = -2, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = -1, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 1, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 2, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 3, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_boxplot(width = 0.75, lwd = 0.7, stat = "identity", outlier.alpha = 0, fill = "#A4A4A4") +
  scale_x_discrete(limits=c("7", "6", "3", "5", "1", "2", "4"),
                   labels = c("Bare Areas", "Shrubland/Sparse Vegetation", "Grassland", "Urban", 
                              "Agriculture", "Forest", "Wetland")) +
  # stat_summary(data = test, aes(y = GRACE, group = 1), fun.y = mean, geom = "line", colour = "black", size = 1.6) +
  # stat_summary(aes(y = GRACE, group = 2), fun.y = mean, geom = "point", colour = "black", size = 1.6) +
  # stat_summary(aes(y = GRACE, group = 3), fun.y = mean, geom = "point", colour = "black", size = 1.6) +
  # geom_point(data = GRACExcrop_summary_no1, aes(x = Crop_bins0d5, y = GRACE_upright + sd), size = 2) +
  # geom_point(data = GRACExcrop_summary_no1, aes(x = Crop_bins0d5, y = GRACE_upright - sd), size = 2) +
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
  coord_flip(ylim = c(-2, 2)) +
  ylab("GRACE") + xlab("LandUseCat")
Figure2d

ggsave("C:/Users/Tom/Desktop/CellArea_figures/LandUse_AreaWeighted.png", figure, 
       dpi = 500, width = 6, height = 7, bg = "transparent")
