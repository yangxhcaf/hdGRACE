library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(ggspatial)
library(ggplot2)
library(e1071)
library(scales)
library(magrittr)
library(reshape2)
library(RColorBrewer)
library(Hmisc)
library(quantreg)
library(spatstat)
library(reldist)

# load water security vulnerability distributions
WSV_FLOOD <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/WSV_FLOOD.tif")
WSV_STRESS <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/WSV_STRESS.tif")
# make sure both have same extents
WSV_FLOOD[is.na(WSV_STRESS)] <- NA
WSV_STRESS[is.na(WSV_FLOOD)] <- NA

# load raster cell area in km2 at 0d05 resolution
CellArea <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/WGS84_cellArea_0d05res.tif")

# import MAgPIE world regions
MAgPIE_ras <- raster("Z:/2.active_projects/Xander/! GIS_files/WorldRegions/MAgPIE_worldregions.tif")

# Initialize rasters to classify the percentile distribution
WSV_stress_quantile <- raster(CellArea)
WSV_flood_quantile <- raster(CellArea)

# create data frames for area weighted quantiles to be calculated with
WSV_STRESS_df <- cbind(as.data.frame(WSV_STRESS), as.data.frame(CellArea), as.data.frame(MAgPIE_ras))
colnames(WSV_STRESS_df) <- c("WSV_Stress", "Area", "MAgPIE")
WSV_FLOOD_df <- cbind(as.data.frame(WSV_FLOOD), as.data.frame(CellArea), as.data.frame(MAgPIE_ras))
colnames(WSV_FLOOD_df) <- c("WSV_Flood", "Area", "MAgPIE")

WSV_STRESS_India_df <- WSV_STRESS_df %>% filter(MAgPIE == 6)
WSV_FLOOD_India_df <- WSV_FLOOD_df %>% filter(MAgPIE == 6)

# Create a function to create a global raster which takes a global distribution raster and reclassifies it by percentile
PercentileAssignment <- function(QuantileRaster, RawRaster, DIS_Quantile_df){
  QuantileRaster[] <- 0
  QuantileRaster[RawRaster > as.numeric(unname(weighted.quantile(DIS_Quantile_df[,1], DIS_Quantile_df[,2], 0.99, na.rm = TRUE)))] <- 1
  j = 1
  k = 0.99
  for(i in 1:98){
    j = j - 0.01
    k = k - 0.01
    QuantileRaster[RawRaster <= as.numeric(unname(weighted.quantile(DIS_Quantile_df[,1], DIS_Quantile_df[,2], j, na.rm = TRUE))) & 
                     RawRaster > as.numeric(unname(weighted.quantile(DIS_Quantile_df[,1], DIS_Quantile_df[,2], k, na.rm = TRUE)))] <- j
  }
  QuantileRaster[RawRaster < as.numeric(unname(weighted.quantile(DIS_Quantile_df[,1], DIS_Quantile_df[,2], 0.01, na.rm = TRUE)))] <- 0
  return(QuantileRaster)
}

# Reclassify population raster based on quantile using above function
WSV_stress_quantile <- PercentileAssignment(WSV_stress_quantile, WSV_STRESS, WSV_STRESS_India_df)
WSV_flood_quantile <- PercentileAssignment(WSV_flood_quantile, WSV_FLOOD, WSV_FLOOD_India_df)

WSV_stress_quantile_noNA <- WSV_stress_quantile
WSV_stress_quantile_noNA[is.na(WSV_STRESS[])] <- NA
WSV_stress_quantile_noNA[is.na(WSV_FLOOD[])] <- NA

WSV_flood_quantile_noNA <- WSV_flood_quantile
WSV_flood_quantile_noNA[is.na(WSV_STRESS[])] <- NA
WSV_flood_quantile_noNA[is.na(WSV_FLOOD[])] <- NA

MAgPIE.WSV_quantile_distr <- cbind(as.data.frame(WSV_stress_quantile_noNA), as.data.frame(WSV_flood_quantile_noNA),
                                   as.data.frame(MAgPIE_ras), as.data.frame(CellArea))
MAgPIE.WSV_quantile_distr <- MAgPIE.WSV_quantile_distr[complete.cases(MAgPIE.WSV_quantile_distr), ]
colnames(MAgPIE.WSV_quantile_distr) <- c("WSV.stress.quantile", "WSV.flood.quantile","MAgPIE.id", "Area")
MAgPIE.WSV_quantile_distr$MAgPIE.id %<>% as.factor()

# determine distribution statistics for the coupled raster grids
summ_WSV_stress_quantile <- MAgPIE.WSV_quantile_distr %>% 
  group_by(MAgPIE.id) %>%
  summarise(WeightedMean = weighted.mean(WSV.stress.quantile, Area),
            WeightedMedian = weighted.quantile(WSV.stress.quantile, Area, probs = 0.50, na.rm = TRUE),
            Weightedp25 = weighted.quantile(WSV.stress.quantile, Area, probs = 0.25, na.rm = TRUE),
            Weightedp75 = weighted.quantile(WSV.stress.quantile, Area, probs = 0.75, na.rm = TRUE),
            WeightedpLOW = weighted.quantile(WSV.stress.quantile, Area, probs = 0.05, na.rm = TRUE),
            WeightedpHIGH = weighted.quantile(WSV.stress.quantile, Area, probs = 0.95, na.rm = TRUE))

summ_WSV_flood_quantile <- MAgPIE.WSV_quantile_distr %>% 
  group_by(MAgPIE.id) %>%
  summarise(WeightedMean = weighted.mean(WSV.stress.quantile, Area),
            WeightedMedian = weighted.quantile(WSV.flood.quantile, Area, probs = 0.50, na.rm = TRUE),
            Weightedp25 = weighted.quantile(WSV.flood.quantile, Area, probs = 0.25, na.rm = TRUE),
            Weightedp75 = weighted.quantile(WSV.flood.quantile, Area, probs = 0.75, na.rm = TRUE),
            WeightedpLOW = weighted.quantile(WSV.flood.quantile, Area, probs = 0.05, na.rm = TRUE),
            WeightedpHIGH = weighted.quantile(WSV.flood.quantile, Area, probs = 0.95, na.rm = TRUE))

# 
summ_DF_stress <- data.frame(x= summ_WSV_stress_quantile$MAgPIE.id, min=summ_WSV_stress_quantile$WeightedMedian, 
                             low=summ_WSV_stress_quantile$WeightedMedian, 
                             mid = summ_WSV_stress_quantile$WeightedMedian, 
                             top=summ_WSV_stress_quantile$Weightedp75, max= summ_WSV_stress_quantile$WeightedpHIGH)
summ_DF_stress$x %<>% as.factor()
summ_DF_stress$WSVtype <- "stress"

summ_DF_flood <- data.frame(x= summ_WSV_flood_quantile$MAgPIE.id, min=summ_WSV_flood_quantile$WeightedMedian, 
                             low=summ_WSV_flood_quantile$WeightedMedian, 
                             mid = summ_WSV_flood_quantile$WeightedMedian, 
                             top=summ_WSV_flood_quantile$Weightedp75, max= summ_WSV_flood_quantile$WeightedpHIGH)
summ_DF_flood$x %<>% as.factor()
summ_DF_flood$WSVtype <- "Flood"

summ_DF <- rbind(summ_DF_stress, summ_DF_flood)
summ_DF$min %<>% round(digits = 2)
summ_DF$low %<>% round(digits = 2)
summ_DF$mid %<>% round(digits = 2)
summ_DF$top <- ifelse(summ_DF$WSVtype == "Flood", trunc(summ_DF$top*100)/100, summ_DF$top)

# 1 - NAM | 2 - FSU | 3 - POECD | 4 - PAS | 5 - CPA | 6 - SAS | 7 - SSA | 8 _ MENA | 9 - WEUR | 10 - LAM 
FigInsert <- ggplot(summ_DF, aes(x = x, ymin = min, lower = low, middle = mid, upper = top, ymax = max, fill = factor(WSVtype))) +
  # scale_x_discrete(limsits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  geom_boxplot(stat = "identity") +
  # scale_y_continuous(breaks = seq(-3, 3, by = 1), expand = c(0,0)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size= 1.5),
        axis.title = element_text(size = 11, color = "black"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        # panel.grid.major.y = element_line(alpha(colour = "white"), linetype = "dashed", size = 0.5),
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(face = "bold", size = 10, color = "black"),
        axis.title.y = element_text(color = "black"))+
  scale_fill_manual(values = c("dodgerblue4","firebrick3"))+
  coord_flip(ylim = c(0, 1))
FigInsert

ggsave("C:/Users/Tom/Desktop/SAS_normalize_WSVresults.png", FigInsert, dpi = 500, width = 10, height = 5, bg = "transparent")
