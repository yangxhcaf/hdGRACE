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

# load population and individual water security vulnerability distributions
POPULATION2015 <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/POP_2015_0d05.tif")
WSVI_FLOOD <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/WSVI_FLOOD.tif")
WSVI_STRESS <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/WSVI_STRESS.tif")

# Determine WSVI scores per MAgPIE regions
MAgPIE_ras <- raster("Z:/2.active_projects/Xander/! GIS_files/WorldRegions/MAgPIE_worldregions.tif")

MAgPIE_ras_df <- MAgPIE_ras %>% as.data.frame()
WSVI_FLOOD_df <- WSVI_FLOOD %>% as.data.frame()
WSVI_STRESS_df <- WSVI_STRESS %>% as.data.frame()

MAgPIE_WSVI <- cbind(MAgPIE_ras_df,WSVI_FLOOD_df, WSVI_STRESS_df)
MAgPIE_WSVI <- MAgPIE_WSVI[complete.cases(MAgPIE_WSVI), ]
MAgPIE_WSVI$MAgPIE_worldregions %<>% as.factor()
test <- melt(MAgPIE_WSVI, id.vars = "MAgPIE_worldregions")

### below is box plot
MAgPIE_WSVI_boxplot <- ggplot(test, aes(x=MAgPIE_worldregions, y = value, fill = variable)) +
  # scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  geom_boxplot(outlier.alpha = 0, 
               # coef = 0, 
               na.rm = TRUE) +
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
   scale_fill_manual(values = c("dodgerblue4", "firebrick3"))+
  coord_flip(ylim = c(0, 5))
MAgPIE_WSVI_boxplot

# 1 - NAM | 2 - FSU | 3 - POECD | 4 - PAS | 5 - CPA | 6 - SAS | 7 - SSA | 8 _ MENA | 9 - WEUR | 10 - LAM 

ggsave("C:/Users/Tom/Desktop/MAgPIE_WSVI_boxplot_1.png", MAgPIE_WSVI_boxplot, width = 10, height = 5, bg = "transparent")
