library(raster)
library(rgdal)
library(dplyr)
library(ggspatial)
library(ggplot2)
library(e1071)
library(scales)
library(reshape2)
library(naniar)
library(RColorBrewer)
library(Rmisc)

### Load Data

# Load GRACE DATA (at 0.5 d resolution)
GRACE <- raster("C:/Users/lab/Documents/! Xander/! Research/TWS/GRACE_upright.tif")

# Resample GRACE to 0.05d resolution
s <- raster(nrow=3600, ncol=7200)
GRACE_0d05res <- resample(GRACE, s, method='bilinear')
GRACE_0d05res <- raster("Z:/2.active_projects/Xander/! GIS_files/GRACE/GRACE_coredata.tif")

# Load biodiversity data 

# amphibian species richness 
AmphSpRch <- raster("Z:/2.active_projects/Xander/! GIS_files/Biodiversity/Raster/Amph_SpeciesRichness/Richness_10km_AMPHIBIANS.tif")
# Resample GRACE to 0.05d resolution
AmphSpRch_0d05res <- resample(AmphSpRch, GRACE_0d05res, method='bilinear')
P10 <- raster::quantile(AmphSpRch_0d05res, probs = 0.1, type = 7, na.rm = TRUE, names = FALSE)
P20 <- raster::quantile(AmphSpRch_0d05res, probs = 0.2, type = 7, na.rm = TRUE, names = FALSE)
P30 <- raster::quantile(AmphSpRch_0d05res, probs = 0.3, type = 7, na.rm = TRUE, names = FALSE)
P40 <- raster::quantile(AmphSpRch_0d05res, probs = 0.4, type = 7, na.rm = TRUE, names = FALSE)
P50 <- raster::quantile(AmphSpRch_0d05res, probs = 0.5, type = 7, na.rm = TRUE, names = FALSE)
P60 <- raster::quantile(AmphSpRch_0d05res, probs = 0.6, type = 7, na.rm = TRUE, names = FALSE)
P70 <- raster::quantile(AmphSpRch_0d05res, probs = 0.7, type = 7, na.rm = TRUE, names = FALSE)
P80 <- raster::quantile(AmphSpRch_0d05res, probs = 0.8, type = 7, na.rm = TRUE, names = FALSE)
P90 <- raster::quantile(AmphSpRch_0d05res, probs = 0.9, type = 7, na.rm = TRUE, names = FALSE)
P100 <- raster::quantile(AmphSpRch_0d05res, probs = 1.0, type = 7, na.rm = TRUE, names = FALSE)

Low <- c(0, P10, P20, P30, P40, P50, P60, P70, P80, P90)
High <- c(P10, P20, P30, P40, P50, P60, P70, P80, P90, P100)
Reclss <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

ReclssTable <- cbind(Low, High, Reclss) %>% as.data.frame() 
names(ReclssTable) <- NULL

AmphSpRch_0d05res_bins <- reclassify(AmphSpRch_0d05res,     
                                     ReclssTable) 

# Merge GRACE data with binned species richness
GRACE_df <- GRACE_0d05res %>% as.data.frame()
AmphSpRch_0d05res_bins_df <- AmphSpRch_0d05res_bins %>% as.data.frame()
GRACE_per_BiodiversitySpRich <- cbind(GRACE_df, AmphSpRch_0d05res_bins_df)
colnames(GRACE_per_BiodiversitySpRich) <- c("GRACE", "Amph")
GRACE_per_BiodiversitySpRich <- GRACE_per_BiodiversitySpRich[complete.cases(GRACE_per_BiodiversitySpRich), ]
GRACE_per_BiodiversitySpRich$Amph <- as.factor(GRACE_per_BiodiversitySpRich$Amph)

# coul = colorRampPalette(coul)(4)
detach(package:Rmisc)
detach(package:plyr)
summ <- GRACE_per_BiodiversitySpRich %>% 
  group_by(Amph) %>%
  summarize(mean = mean(GRACE), median = median(GRACE), IQR = IQR(GRACE))

### below is box plot
GRACE_per_BiodiversitySpRich_boxplot <- ggplot(GRACE_per_BiodiversitySpRich, aes(x=Amph, y = GRACE)) +
  geom_hline(yintercept = 0, size = 1.5) +
  geom_hline(yintercept = -3, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = -2, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = -1, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 1, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 2, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 3, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_boxplot(outlier.alpha = 0, fill = "#A4A4A4") +
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
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
  coord_flip(ylim = c(-2, 2))
GRACE_per_BiodiversitySpRich_boxplot 

ggsave("C:/Users/Tom/Desktop/AmphDistr_10th.png", GRACE_per_BiodiversitySpRich_boxplot, width = 5, height = 7, dpi = 300, bg = "transparent")

