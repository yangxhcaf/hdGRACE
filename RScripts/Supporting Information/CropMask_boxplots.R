library(raster)
library(rgdal)
library(dplyr)
library(ggspatial)
library(ggplot2)
library(e1071)
library(scales)
library(sp)
library(Hmisc)
library(quantreg)
library(spatstat)
library(reldist)

### Load Data

# Import Rodell et al. (2018) emerging freshwater availability trend as done in previous scripts
EmergingTrend <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/GRACE_0d05.tif")

# Load crop mask layer (raster)
CropMask <- raster("Z:/2.active_projects/Xander/! GIS_files/Cropland/GFSAD/!Mask/GFSAD1KCM.2010.001__0d05.tif")

# load raster cell area in km2 at 0d05 resolution
CellArea <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/WGS84_cellArea_0d05res.tif")
CellArea_norm <- CellArea/max(CellArea[])

# Merge GRACE data with binned species richness
EmergingTrend_df <- EmergingTrend %>% as.data.frame()
CropMask_df <- CropMask %>% as.data.frame()
CellArea_norm_df <- CellArea_norm %>% as.data.frame()
CropMask_analysis <- cbind(GRACE_df, CropMask_df, CellArea_norm_df)
colnames(CropMask_analysis) <- c("GRACE", "CropMask", "AreaWgt")
CropMask_analysis <- CropMask_analysis[complete.cases(CropMask_analysis), ]
CropMask_analysis$CropMask <- as.factor(CropMask_analysis$CropMask)

# determine distribution statistics for the coupled raster grids
summ <- CropMask_analysis %>% 
  group_by(CropMask) %>%
  summarise(WeightedMean = weighted.mean(GRACE, AreaWgt),
            WeightedMedian = weighted.median(GRACE, AreaWgt),
            Weightedp25 = weighted.quantile(GRACE, AreaWgt, probs = 0.25),
            Weightedp75 = weighted.quantile(GRACE, AreaWgt, probs = 0.75),
            WeightedLOW = weighted.quantile(GRACE, AreaWgt, probs = 0.05),
            WeightedHIGH = weighted.quantile(GRACE, AreaWgt, probs = 0.95))
summ$WeightedIQR <- summ$Weightedp75 - summ$Weightedp25

# format results for ggplot boxplot figure generation 
summDF <- data.frame(x= summ$CropMask, min=summ$WeightedLOW, 
                     low=summ$Weightedp25, wgt.mean = summ$WeightedMean, mid = summ$WeightedMedian, 
                     top=summ$Weightedp75, max= summ$WeightedHIGH)
summDF <- summDF %>% filter(x != "9") # remove non agricultural areas

## below is area weighted box plot
SI.Figure2b <-  ggplot(summDF, aes(x = x, ymin = min, lower = low, middle = mid, upper = top, ymax = max, fill = x)) +
  geom_hline(yintercept = 0, size = 1.5) +
  geom_hline(yintercept = -3, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = -2, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = -1, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 1, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 2, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 3, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_boxplot(width = 0.75, lwd = 0.7, stat = "identity", outlier.alpha = 0, fill = c("#0000ff", "#3261bf", "#008080", "#00bf40", "#00ff00")) +
  scale_x_discrete(limits=c("1", "2", "3", "4", "5")) +
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
SI.Figure2b 

ggsave("C:/Users/Tom/Desktop/CellArea_figures/CropMask_AreaWeighted.png", SI.Figure2b, 
       dpi = 500, width = 6, height = 7, bg = "transparent")


# Now determine spatial breakdown of each crop mask category in developing ternary plots
CropMask_analysis$TREND <- ifelse(CropMask_analysis$GRACE > 0.5, "W", 
                                   ifelse(CropMask_analysis$GRACE < -0.5, "D", "S"))
CropMask_analysis$CrpMsk_ArTrend <- CropMask_analysis$GRACE * CropMask_analysis$AreaWgt


# CM 1
CM1_wet <- CropMask_analysis %>% filter(CropMask == 1 & TREND == "W") %>% pull(AreaWgt) %>% sum()
CM1_stb <- CropMask_analysis %>% filter(CropMask == 1 & TREND == "S") %>% pull(AreaWgt) %>% sum()
CM1_dry <- CropMask_analysis %>% filter(CropMask == 1 & TREND == "D") %>% pull(AreaWgt) %>% sum()
CM1_ar <- CropMask_analysis %>% filter(CropMask == 1) %>% pull(AreaWgt) %>% sum()

# CM 2
CM2_wet <- CropMask_analysis %>% filter(CropMask == 2 & TREND == "W") %>% pull(AreaWgt) %>% sum()
CM2_stb <- CropMask_analysis %>% filter(CropMask == 2 & TREND == "S") %>% pull(AreaWgt) %>% sum()
CM2_dry <- CropMask_analysis %>% filter(CropMask == 2 & TREND == "D") %>% pull(AreaWgt) %>% sum()
CM2_ar <- CropMask_analysis %>% filter(CropMask == 2) %>% pull(AreaWgt) %>% sum()

# CM 3
CM3_wet <- CropMask_analysis %>% filter(CropMask == 3 & TREND == "W") %>% pull(AreaWgt) %>% sum()
CM3_stb <- CropMask_analysis %>% filter(CropMask == 3 & TREND == "S") %>% pull(AreaWgt) %>% sum()
CM3_dry <- CropMask_analysis %>% filter(CropMask == 3 & TREND == "D") %>% pull(AreaWgt) %>% sum()
CM3_ar <- CropMask_analysis %>% filter(CropMask == 3) %>% pull(AreaWgt) %>% sum()

# CM 4
CM4_wet <- CropMask_analysis %>% filter(CropMask == 4 & TREND == "W") %>% pull(AreaWgt) %>% sum()
CM4_stb <- CropMask_analysis %>% filter(CropMask == 4 & TREND == "S") %>% pull(AreaWgt) %>% sum()
CM4_dry <- CropMask_analysis %>% filter(CropMask == 4 & TREND == "D") %>% pull(AreaWgt) %>% sum()
CM4_ar <- CropMask_analysis %>% filter(CropMask == 4) %>% pull(AreaWgt) %>% sum()

# CM 5
CM5_wet <- CropMask_analysis %>% filter(CropMask == 5 & TREND == "W") %>% pull(AreaWgt) %>% sum()
CM5_stb <- CropMask_analysis %>% filter(CropMask == 5 & TREND == "S") %>% pull(AreaWgt) %>% sum()
CM5_dry <- CropMask_analysis %>% filter(CropMask == 5 & TREND == "D") %>% pull(AreaWgt) %>% sum()
CM5_ar <- CropMask_analysis %>% filter(CropMask == 5) %>% pull(AreaWgt) %>% sum()

# Wet percent
WtPCT <- c(CM1_wet/(CM1_wet+CM1_stb+CM1_dry),
           CM2_wet/(CM2_wet+CM2_stb+CM2_dry),
           CM3_wet/(CM3_wet+CM3_stb+CM3_dry),
           CM4_wet/(CM4_wet+CM4_stb+CM4_dry),
           CM5_wet/(CM5_wet+CM5_stb+CM5_dry))
# Stable percent
StbPCT <- c(CM1_stb/(CM1_wet+CM1_stb+CM1_dry),
           CM2_stb/(CM2_wet+CM2_stb+CM2_dry),
           CM3_stb/(CM3_wet+CM3_stb+CM3_dry),
           CM4_stb/(CM4_wet+CM4_stb+CM4_dry),
           CM5_stb/(CM5_wet+CM5_stb+CM5_dry))

# Dry percent
DryPCT <- c(CM1_dry/(CM1_wet+CM1_stb+CM1_dry),
            CM2_dry/(CM2_wet+CM2_stb+CM2_dry),
            CM3_dry/(CM3_wet+CM3_stb+CM3_dry),
            CM4_dry/(CM4_wet+CM4_stb+CM4_dry),
            CM5_dry/(CM5_wet+CM5_stb+CM5_dry))

Count <- c(CM1_ar,
           CM2_ar,
           CM3_ar,
           CM4_ar,
           CM5_ar)

SummMatrix <- cbind(WtPCT, StbPCT, DryPCT, Count)

# Cell count

rownames(SummMatrix) <- c("1", "2", "3", "4", "5")



library(Ternary)
tiff('C:/Users/Tom/Desktop/Ternary_CropMask.tiff', units="in", width=7, height=7, res=500, compression = 'lzw')

TernaryPlot(alab="\u2190 % Drying", blab="\u2190 % Stable", clab="% Wetting \u2192",
            point='down', 
            # scale = 4,
            grid.lines=5, grid.lty='dotted',
            grid.minor.lines=1, grid.minor.lty='dotted')

# add data points
data_points <- list(
  V   = c(SummMatrix[5,3], SummMatrix[5,2], SummMatrix[5,1]),
  IV  = c(SummMatrix[4,3], SummMatrix[4,2], SummMatrix[4,1]),
  III = c(SummMatrix[3,3], SummMatrix[3,2], SummMatrix[3,1]),
  II  = c(SummMatrix[2,3], SummMatrix[2,2], SummMatrix[2,1]),
  I   = c(SummMatrix[1,3], SummMatrix[1,2], SummMatrix[1,1])
)

alpha = 3
scaleFACTOR <- c(alpha*sqrt(SummMatrix[5,4]/SummMatrix[2,4]),
                 alpha*sqrt(SummMatrix[4,4]/SummMatrix[2,4]),
                 alpha*sqrt(SummMatrix[3,4]/SummMatrix[2,4]),
                 alpha*sqrt(SummMatrix[2,4]/SummMatrix[2,4]),
                 alpha*sqrt(SummMatrix[1,4]/SummMatrix[2,4])
                 )

AddToTernary(points, data_points, pch = 21, cex = scaleFACTOR, bg = rev(c("#0000ff", "#3261bf", "#008080", "#00bf40", "#00ff00")))

dev.off()
