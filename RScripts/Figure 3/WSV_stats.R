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

# load raster cell area in km2 at 0d05 resolution
CellArea <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/WGS84_cellArea_0d05res.tif")
CellArea_norm <- CellArea/max(CellArea[])

# import MAgPIE world regions
MAgPIE_ras <- raster("Z:/2.active_projects/Xander/! GIS_files/WorldRegions/MAgPIE_worldregions.tif")

# assess distribution of results per MAgPIE region

# FLOOD
WSV_FLOOD_df <- WSV_FLOOD %>% as.data.frame()
WSV_STRESS_df <- WSV_STRESS %>% as.data.frame()
MAgPIE_df <- MAgPIE_ras %>% as.data.frame()
CellArea_norm_df <- CellArea_norm %>% as.data.frame()

MAgPIE.distr <- cbind(WSV_FLOOD_df, WSV_STRESS_df, MAgPIE_df, CellArea_norm_df)
MAgPIE.distr <- MAgPIE.distr[complete.cases(MAgPIE.distr), ]
colnames(MAgPIE.distr) <- c("WSV.flood", "WSV.stress","MAgPIE.id", "AreaWgt")
MAgPIE.distr$MAgPIE.id %<>% as.factor()

# determine distribution statistics for the coupled raster grids
summ_flood <- MAgPIE.distr %>% 
  group_by(MAgPIE.id) %>%
  summarise(WeightedMean = weighted.mean(WSV.flood, AreaWgt),
            WeightedMedian = weighted.median(WSV.flood, AreaWgt),
            Weightedp25 = weighted.quantile(WSV.flood, AreaWgt, probs = 0.25),
            Weightedp75 = weighted.quantile(WSV.flood, AreaWgt, probs = 0.75),
            WeightedpLOW = weighted.quantile(WSV.flood, AreaWgt, probs = 0.05),
            WeightedpHIGH = weighted.quantile(WSV.flood, AreaWgt, probs = 0.95))
summ_flood$WeightedIQR <- summ_flood$Weightedp75 - summ_flood$Weightedp25

summ_stress <- MAgPIE.distr %>% 
  group_by(MAgPIE.id) %>%
  summarise(WeightedMean = weighted.mean(WSV.stress, AreaWgt),
            WeightedMedian = weighted.median(WSV.stress, AreaWgt),
            Weightedp25 = weighted.quantile(WSV.stress, AreaWgt, probs = 0.25),
            Weightedp75 = weighted.quantile(WSV.stress, AreaWgt, probs = 0.75),
            WeightedpLOW = weighted.quantile(WSV.stress, AreaWgt, probs = 0.05),
            WeightedpHIGH = weighted.quantile(WSV.stress, AreaWgt, probs = 0.95))
summ_stress$WeightedIQR <- summ_stress$Weightedp75 - summ_stress$Weightedp25


summ_DF_flood <- data.frame(x= summ_flood$MAgPIE.id, min=summ_flood$WeightedpLOW, 
                            low=summ_flood$Weightedp25, wgt.mean = summ_flood$WeightedMean, mid = summ_flood$WeightedMedian, 
                            top=summ_flood$Weightedp75, max= summ_flood$WeightedpHIGH)
summ_DF_flood$x %<>% as.factor()
summ_DF_flood$WSVtype <- "flood"

summ_DF_stress <- data.frame(x= summ_stress$MAgPIE.id, min=summ_stress$WeightedpLOW, 
                            low=summ_stress$Weightedp25, wgt.mean = summ_stress$WeightedMean, mid = summ_stress$WeightedMedian, 
                            top=summ_stress$Weightedp75, max= summ_stress$WeightedpHIGH)
summ_DF_stress$x %<>% as.factor()
summ_DF_stress$WSVtype <- "stress"

summ_DF <- rbind(summ_DF_flood, summ_DF_stress)

FigInsert <- ggplot(summ_DF, aes(x = x, ymin = min, lower = low, middle = mid, upper = top, ymax = max, fill = factor(WSVtype))) +
  # scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
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
  scale_fill_manual(values = c("dodgerblue4", "firebrick3"))+
  coord_flip(ylim = c(0, 5))
FigInsert

# 1 - NAM | 2 - FSU | 3 - POECD | 4 - PAS | 5 - CPA | 6 - SAS | 7 - SSA | 8 _ MENA | 9 - WEUR | 10 - LAM 

ggsave("C:/Users/Tom/Desktop/MAgPIE_WSV_summary.png", FigInsert, dpi = 500, width = 10, height = 5, bg = "transparent")

# calculate country rankings
#
#
# import Country shapefile (add associated attribute table)
Country.shp <- readOGR(dsn = "Z:/2.active_projects/Xander/! GIS_files/NaturalEarth",
                       layer = "ne_10m_admin_0_countries")
keepcols <- c("SOVEREIGNT", "SOV_A3", "ADM0_A3", "NAME")
Country.shp <- Country.shp[,keepcols, drop = FALSE]
Country.shp <- Country.shp[order(Country.shp$ADM0_A3),]
# Create unique integer ID for each country
Country.shp$ID <- seq(1, nrow(Country.shp), 1)

# import country extents IDed to correspond to attribute table
CountryID <- raster("Z:/2.active_projects/Xander/! GIS_files/VirtualWater/CountryID.tif")

WSV_FLOOD_df <- WSV_FLOOD %>% as.data.frame()
WSV_STRESS_df <- WSV_STRESS %>% as.data.frame()
CountryID_df <- CountryID %>% as.data.frame()
CellArea_norm_df <- CellArea_norm %>% as.data.frame()

Country_WSV <- cbind(WSV_FLOOD_df, WSV_STRESS_df, CountryID_df, CellArea_norm_df)
Country_WSV <- Country_WSV[complete.cases(Country_WSV), ]
colnames(Country_WSV) <- c("WSV.flood", "WSV.stress","Country_ID", "AreaWgt")
Country_WSV$Country_ID %<>% as.factor()

# Country stats for Flooding
summ_WSV_country_flood <- Country_WSV %>% 
  group_by(Country_ID) %>%
  summarise(WeightedMean = weighted.mean(WSV.flood, AreaWgt),
            WeightedMedian = weighted.median(WSV.flood, AreaWgt),
            Weightedp25 = weighted.quantile(WSV.flood, AreaWgt, probs = 0.25),
            Weightedp75 = weighted.quantile(WSV.flood, AreaWgt, probs = 0.75),
            WeightedLOW = weighted.quantile(WSV.flood, AreaWgt, probs = 0.05),
            WeightedHIGH = weighted.quantile(WSV.flood, AreaWgt, probs = 0.95))
summ_WSV_country_flood$WeightedIQR <- summ_WSV_country_flood$Weightedp75 - summ_WSV_country_flood$Weightedp25
summ_WSV_country_flood %<>% as.data.frame()
# Merge results with Country ID table
summ_WSV_country_flood <- merge.data.frame(summ_WSV_country_flood, Country.shp, by.x = "Country_ID", by.y = "ID", all = FALSE)
summ_WSV_country_flood <- summ_WSV_country_flood[order(-summ_WSV_country_flood$Weightedp75),]
summ_WSV_country_flood$Rank <- c(seq(1, nrow(summ_WSV_country_flood), by = 1))

summ_WSV_country_flood$Low <- summ_WSV_country_flood$Weightedp25 - summ_WSV_country_flood$WeightedIQR
summ_WSV_country_flood$Low <- ifelse(summ_WSV_country_flood$Low<0, 0, summ_WSV_country_flood$Low)
summ_WSV_country_flood$High <- summ_WSV_country_flood$Weightedp75 + summ_WSV_country_flood$WeightedIQR
summ_WSV_country_flood$NAME <- ifelse(summ_WSV_country_flood$NAME == "CuraÃ§ao",
                                      "Curacao",
                                      as.character(summ_WSV_country_flood$NAME))

CountryFlood <- ggplot(summ_WSV_country_flood, aes(x = Rank, ymin = WeightedLOW, lower = Weightedp25, 
                                                   middle = WeightedMedian, upper = Weightedp75, ymax = WeightedHIGH)) +
  geom_hline(yintercept = 0, size = 1.5) +
  geom_boxplot(width = 0.75, lwd = 0.7, stat = "identity", outlier.alpha = 0, fill = "lightblue3") +
  # geom_text(aes(y = 6.5, label = NAME))+
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
  scale_x_reverse(breaks=c(seq(1,29,1)), labels=c(as.character(summ_WSV_country_flood$NAME[1:29])))+
  coord_flip(xlim=c(0.5,29.5), ylim=c(0,5), expand = c(0,0))
CountryFlood
ggsave("C:/Users/Tom/Desktop/Flood_CountryRank.png", CountryFlood, dpi = 500, width = 8, height = 8, bg = "transparent")

# now repeat for stress
summ_WSV_country_stress <- Country_WSV %>% 
  group_by(Country_ID) %>%
  summarise(WeightedMean = weighted.mean(WSV.stress, AreaWgt),
            WeightedMedian = weighted.median(WSV.stress, AreaWgt),
            Weightedp25 = weighted.quantile(WSV.stress, AreaWgt, probs = 0.25),
            Weightedp75 = weighted.quantile(WSV.stress, AreaWgt, probs = 0.75),
            WeightedLOW = weighted.quantile(WSV.stress, AreaWgt, probs = 0.05),
            WeightedHIGH = weighted.quantile(WSV.stress, AreaWgt, probs = 0.95))
summ_WSV_country_stress$WeightedIQR <- summ_WSV_country_stress$Weightedp75 - summ_WSV_country_stress$Weightedp25
summ_WSV_country_stress %<>% as.data.frame()
# Merge results with Country ID table
summ_WSV_country_stress <- merge.data.frame(summ_WSV_country_stress, Country.shp, by.x = "Country_ID", by.y = "ID", all = FALSE)
summ_WSV_country_stress <- summ_WSV_country_stress[order(-summ_WSV_country_stress$Weightedp75),]
summ_WSV_country_stress <- summ_WSV_country_stress %>% filter(NAME != "Akrotiri" &
                                                                NAME != "Cyprus U.N. Buffer Zone" &
                                                                NAME != "Dhekelia")
summ_WSV_country_stress$Rank <- c(seq(1, nrow(summ_WSV_country_stress), by = 1))

summ_WSV_country_stress$Low <- summ_WSV_country_stress$Weightedp25 - summ_WSV_country_stress$WeightedIQR
summ_WSV_country_stress$Low <- ifelse(summ_WSV_country_stress$Low<0, 0, summ_WSV_country_stress$Low)
summ_WSV_country_stress$High <- summ_WSV_country_stress$Weightedp75 + summ_WSV_country_stress$WeightedIQR


# summ_WSV_country_stress$NAME <- ifelse(summ_WSV_country_flood$NAME == "CuraÃ§ao",
#                                       "Curacao",
#                                       as.character(summ_WSV_country_flood$NAME))

CountryStress <- ggplot(summ_WSV_country_stress, aes(x = Rank, ymin = WeightedLOW, lower = Weightedp25, 
                                                   middle = WeightedMedian, upper = Weightedp75, ymax = WeightedHIGH)) +
  geom_hline(yintercept = 0, size = 1.5) +
  geom_boxplot(width = 0.75, lwd = 0.7, stat = "identity", outlier.alpha = 0, fill = "red") +
  # geom_text(aes(y = 6.5, label = NAME))+
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
  scale_x_reverse(breaks=c(seq(1,29,1)), labels=c(as.character(summ_WSV_country_stress$NAME[1:29])))+
  coord_flip(xlim=c(0.5,29.5), ylim=c(0,5), expand = c(0,0))
CountryStress
ggsave("C:/Users/Tom/Desktop/Stress_CountryRank.png", CountryStress, dpi = 500, width = 8, height = 8, bg = "transparent")
