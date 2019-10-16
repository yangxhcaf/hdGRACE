library(raster)
library(dplyr)
library(magrittr)

GRACE_original <- raster("Z:/2.active_projects/Xander/! GIS_files/GRACE/Rodell_raw.tif")
GRACE_bilin <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/GRACE_0d05.tif")

GRACE_original_0d05 <- resample(GRACE_original, GRACE_bilin, method = "ngb")

GRACE_bilin_df <- GRACE_bilin %>% as.data.frame
GRACE_original_0d05_df <- GRACE_original_0d05 %>% as.data.frame

Comparison <- cbind(GRACE_original_0d05_df, GRACE_bilin_df)

Comparison$Error <- 100*(Comparison$GRACE_0d05- Comparison$Rodell_raw)/Comparison$Rodell_raw
Comparison$Diff <- abs(Comparison$GRACE_0d05- Comparison$Rodell_raw)

## below is the code to create and plot multiple histograms at once
# Set the bin size and load the high and low values of the distribution
binSize <- 0.05
highValue <- 4
LowValue <- 0

# this initializes a matrix to populate the histogram results with
FreqMatrix <- matrix(data = NA, nrow = ((highValue-LowValue))/binSize, ncol = 2)

# run a for loop to determine the number of observations within each range for each sample set
for (i in 1:nrow(FreqMatrix)){
  FreqMatrix[i,1] <- (i-1)*binSize
  FreqMatrix[i,2] <- Comparison %>% filter((Diff >= ((i-1)*binSize)) & (Diff < ((i)*binSize))) %>% nrow()
}

# turn the matrix into a datframe for plotting
FreqMatrix %<>% as.data.frame()
colnames(FreqMatrix) <- c("LowLimit", "Count")
a <- sum(FreqMatrix$Count)
FreqMatrix$density <- FreqMatrix$Count/a

# plot the results!
figure <- ggplot(FreqMatrix) +
  geom_bar(aes(x=LowLimit+0.025, y=density), stat = "identity", fill = "black")+
  coord_cartesian(xlim = c(0,4), ylim = c(0,1), expand = c(0,0))+
  theme(panel.background =  element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
figure

ggsave("C:/Users/Tom/Desktop/InterpolateError.png", figure, dpi = 500, width = 8, height = 8, bg = "transparent")


# create absolute differene raster
AbsDiffRAS <- abs(GRACE_bilin-GRACE_original_0d05)

writeRaster(AbsDiffRAS, 
            filename="Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/AbsoluteDiff.tif", 
            format="GTiff", overwrite=TRUE)

summary(Comparison$Diff)
quantile(Comparison$Diff, 0.99, na.rm = TRUE)

library("ggpubr")
cor(Comparison$Rodell_raw, Comparison$GRACE_0d05, method = c("pearson"))
cor(Comparison$Rodell_raw, Comparison$GRACE_0d05, method = c("spearman"))


Prs <- cor.test(Comparison$Rodell_raw, Comparison$GRACE_0d05, method= "pearson")
Spr <- cor.test(Comparison$Rodell_raw, Comparison$GRACE_0d05, method= "spearman")

Prs
