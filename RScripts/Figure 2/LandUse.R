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
library(forcats)

### Load Data

# Load GRACE DATA (at 0.5 d resolution)
GRACE <- raster("Z:/2.active_projects/Xander/! GIS_files/GRACE/Rodell_raw.tif")

# Resample GRACE to 0.05d resolution
s <- raster(nrow=3600, ncol=7200)
GRACE_0d05res <- resample(GRACE, s, method='bilinear')
GRACE_0d05 <- raster("Z:/2.active_projects/Xander/! GIS_files/GRACE/GRACE_coredata.tif")

e <- extent(-180, 180, -90, 90)

# Load Land Cover 2000 rater
LandUse <- raster("Z:/2.active_projects/Xander/! GIS_files/LandUse/GlobalLandUse_0d05.tif") 

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



# Merge GRACE data with binned species richness
GRACE_df <- GRACE_0d05 %>% as.data.frame()
GRACE_per_LandUseBin_df <- LandUse_CategoryBin %>% as.data.frame()
GRACE_per_LandUseBin_df <- cbind(GRACE_df, GRACE_per_LandUseBin_df)
colnames(GRACE_per_LandUseBin_df) <- c("GRACE", "LandUseCat")
GRACE_per_LandUseBin_df <- GRACE_per_LandUseBin_df[complete.cases(GRACE_per_LandUseBin_df), ]
GRACE_per_LandUseBin_df$LandUseCat <- as.factor(GRACE_per_LandUseBin_df$LandUseCat)

summary(GRACE_per_LandUseBin_df, 10)

#colour brewer
coul = brewer.pal(7, "Pastel1") 

# coul = colorRampPalette(coul)(4)
detach(package:Rmisc)
detach(package:plyr)
summ <- GRACE_per_LandUseBin_df %>% 
  group_by(LandUseCat) %>%
  summarize(mean = mean(GRACE), median = median(GRACE), IQR = IQR(GRACE))

### below is box plot
LandUseBin_boxplot <- ggplot(GRACE_per_LandUseBin_df, aes(x=LandUseCat, y = GRACE)) +
  geom_hline(yintercept = 0, size = 1.5) +
  geom_hline(yintercept = -3, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = -2, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = -1, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 1, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 2, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 3, size = 0.5, colour = "white", alpha = 0.5, linetype = "dashed") +
  geom_boxplot(outlier.alpha = 0, fill = "#A4A4A4") +
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
  coord_flip(ylim = c(-2, 2))
LandUseBin_boxplot

ggsave("C:/Users/Tom/Desktop/LandUseBin_boxplot.png",
       LandUseBin_boxplot, width = 6, height = 7, dpi = 300, bg = "transparent")


#
# Rank1 <- test1 %>% filter(Taxa == "Comb") %>% filter(Rank == 1) %>% pull(GRACE) 
# Rank2 <- test1 %>% filter(Taxa == "Comb") %>% filter(Rank == 2) %>% pull(GRACE) 
# Rank3 <- test1 %>% filter(Taxa == "Comb") %>% filter(Rank == 3) %>% pull(GRACE) 
# Rank4 <- test1 %>% filter(Taxa == "Comb") %>% filter(Rank == 4) %>% pull(GRACE) 
# Rank5 <- test1 %>% filter(Taxa == "Comb") %>% filter(Rank == 5) %>% pull(GRACE)
# Rank6 <- test1 %>% filter(Taxa == "Comb") %>% filter(Rank == 6) %>% pull(GRACE) 
# Rank7 <- test1 %>% filter(Taxa == "Comb") %>% filter(Rank == 7) %>% pull(GRACE) 
# 
# t.test(Rank7, Rank6, "two.sided")
# 
# 
# 
# 
# 
