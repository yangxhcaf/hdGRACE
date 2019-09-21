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

# tiff('C:/Users/Tom/Desktop/AbsERR_hist.tiff', units="in", width=7, height=7, res=300, compression = 'lzw')
hist(Comparison$Diff,
     prob = FALSE,
     main="Absolute error between raw Rodell et al. (6) source data\nand bilinearly interpolated dataset",
     xlab="cm/yr error",
     xlim=c(-0.0,4.5),
     ylim = c(0,0.25e8),
     col="black",
     border="white",
     breaks=c(seq(-10,10, by = 0.1))
     
)
# dev.off()


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

