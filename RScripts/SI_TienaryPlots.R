library(dplyr)
library(rgdal)
library(raster)
library(magrittr)
library(ggplot2)
library(robustbase)
library(Ternary)

# Import emerging water trends
GRACE <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/GRACE_0d05.tif")

# Part 1: Convert WRI aqueduct basin shapefile data to continuous indexes for water stress and flooding
# Load WRI 2019 release of baseline water stress
BWS_cat_2019 <- raster("Z:/2.active_projects/Xander/! GIS_files/Aqueduct/2019_release/BWS_cat.tif")

# Load WRI 2019 release of riverine flooding risk
RFR_cat_2019 <- raster("Z:/2.active_projects/Xander/! GIS_files/Aqueduct/2019_release/RFR_cat.tif")

# Import population distribution
POPULATION2015 <- raster("Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/POP_2015_0d05.tif")

# Create grace trend bins
GraceTrendBin <- GRACE
GraceTrendBin[GRACE <= -0.5] <- -1
GraceTrendBin[GRACE > -0.5 & GRACE < 0.5] <- 0
GraceTrendBin[GRACE >= 0.5] <- 1
plot(GraceTrendBin)

# Create unique ID for each BWS cat and GRACE trend bin
BWS_GRACE_combinations <- raster(GRACE)

BWS_GRACE_combinations[GraceTrendBin == -1 & BWS_cat_2019 == -1] <- -12
BWS_GRACE_combinations[GraceTrendBin == 0 & BWS_cat_2019 == -1] <- -11
BWS_GRACE_combinations[GraceTrendBin == 1 & BWS_cat_2019 == -1] <- -10

BWS_GRACE_combinations[GraceTrendBin == -1 & BWS_cat_2019 == 0] <- 0
BWS_GRACE_combinations[GraceTrendBin == 0 & BWS_cat_2019 == 0] <- 1
BWS_GRACE_combinations[GraceTrendBin == 1 & BWS_cat_2019 == 0] <- 2

BWS_GRACE_combinations[GraceTrendBin == -1 & BWS_cat_2019 == 1] <- 10
BWS_GRACE_combinations[GraceTrendBin == 0 & BWS_cat_2019 == 1] <- 11
BWS_GRACE_combinations[GraceTrendBin == 1 & BWS_cat_2019 == 1] <- 12

BWS_GRACE_combinations[GraceTrendBin == -1 & BWS_cat_2019 == 2] <- 20
BWS_GRACE_combinations[GraceTrendBin == 0 & BWS_cat_2019 == 2] <- 21
BWS_GRACE_combinations[GraceTrendBin == 1 & BWS_cat_2019 == 2] <- 22

BWS_GRACE_combinations[GraceTrendBin == -1 & BWS_cat_2019 == 3] <- 30
BWS_GRACE_combinations[GraceTrendBin == 0 & BWS_cat_2019 == 3] <- 31
BWS_GRACE_combinations[GraceTrendBin == 1 & BWS_cat_2019 == 3] <- 32

BWS_GRACE_combinations[GraceTrendBin == -1 & BWS_cat_2019 == 4] <- 40
BWS_GRACE_combinations[GraceTrendBin == 0 & BWS_cat_2019 == 4] <- 41
BWS_GRACE_combinations[GraceTrendBin == 1 & BWS_cat_2019 == 4] <- 42
plot(BWS_GRACE_combinations)

# Calculate population within each combination
BWS_GRACE_combinations_Distr <- zonal(POPULATION2015, BWS_GRACE_combinations, "sum")
BWS_GRACE_combinations_Distr %<>% as.data.frame()
colnames(BWS_GRACE_combinations_Distr) <- c("ID", "POP")
head(BWS_GRACE_combinations_Distr)
a <- BWS_GRACE_combinations_Distr
a$ID <- NULL
names(a) <- NULL
a %<>% unlist() %>% as.vector()
a <- matrix(a, nrow = 6, ncol = 3, byrow = TRUE) %>% as.data.frame()
colnames(a) <- c("Pop.Drying", "Pop.Neut", "Pop.Wetting")
rownames(a) <- c("Aird/LowWatUse", "Low", "LowMed", "MedHigh", "High", "ExHigh")
a$PctDRY <- 100 * (a$Pop.Drying) / (a$Pop.Drying + a$Pop.Neut + a$Pop.Wetting)
a$PctNEU <- 100 * (a$Pop.Neut) / (a$Pop.Drying + a$Pop.Neut + a$Pop.Wetting)
a$PctWET <- 100 * (a$Pop.Wetting) / (a$Pop.Drying + a$Pop.Neut + a$Pop.Wetting)
a$TotalPop <- (a$Pop.Drying + a$Pop.Neut + a$Pop.Wetting)

# Ternary plot of stress results
# Initialize Ternary plot
tiff('C:/Users/Tom/Desktop/Ternary_STRESS.tiff', units="in", width=7, height=7, res=300, compression = 'lzw')

TernaryPlot(alab="\u2190 % Drying", blab="\u2190 % Stable", clab="% Wetting \u2192",
            point='down', 
            # scale = 4,
            grid.lines=5, grid.lty='dotted',
            grid.minor.lines=1, grid.minor.lty='dotted')

# add data points
data_points <- list(
  L = c(a[2,1], a[2,2], a[2,3]),
  LM = c(a[3,1], a[3,2], a[3,3]),
  MH = c(a[4,1], a[4,2], a[4,3]),
  H = c(a[5,1], a[5,2], a[5,3]),
  EH = c(a[6,1], a[6,2], a[6,3]),
  A = c(a[1,1], a[1,2], a[1,3])
)

alpha = 3
scaleFACTOR <- c(alpha*sqrt(a[2,7]/a[4,7]), 
                 alpha*sqrt(a[3,7]/a[4,7]), 
                 alpha*sqrt(a[4,7]/a[4,7]), 
                 alpha*sqrt(a[5,7]/a[4,7]), 
                 alpha*sqrt(a[6,7]/a[4,7]),
                 alpha*sqrt(a[1,7]/a[4,7]))
AddToTernary(points, data_points, pch = 21, cex = scaleFACTOR, bg = c('lightgoldenrod1', 
                                                           'yellow', 'orange', 
                                                           'red', 'red4',
                                                           'darkgray'))
# AddToTernary(text, data_points, names(data_points), cex=0.8, font=1)
legend('bottomleft', 
       legend=c('Arid', 'Low', 'LowMed', 'MedHigh', 'High', 'ExHigh'),
       cex=0.8, bty='n', pch=21, pt.cex=1.8,
       pt.bg=c('darkgray', 'lightgoldenrod1', 
                'yellow', 'orange', 
                'red', 'red4')
)

dev.off()

# repeat process, but for riverine flooding risk now

# Create unique ID for each BWS cat and GRACE trend bin
RFR_GRACE_combinations <- raster(GRACE)

RFR_GRACE_combinations[GraceTrendBin == -1 & RFR_cat_2019 == 0] <- 0
RFR_GRACE_combinations[GraceTrendBin == 0 & RFR_cat_2019 == 0] <- 1
RFR_GRACE_combinations[GraceTrendBin == 1 & RFR_cat_2019 == 0] <- 2

RFR_GRACE_combinations[GraceTrendBin == -1 & RFR_cat_2019 == 1] <- 10
RFR_GRACE_combinations[GraceTrendBin == 0 & RFR_cat_2019 == 1] <- 11
RFR_GRACE_combinations[GraceTrendBin == 1 & RFR_cat_2019 == 1] <- 12

RFR_GRACE_combinations[GraceTrendBin == -1 & RFR_cat_2019 == 2] <- 20
RFR_GRACE_combinations[GraceTrendBin == 0 & RFR_cat_2019 == 2] <- 21
RFR_GRACE_combinations[GraceTrendBin == 1 & RFR_cat_2019 == 2] <- 22

RFR_GRACE_combinations[GraceTrendBin == -1 & RFR_cat_2019 == 3] <- 30
RFR_GRACE_combinations[GraceTrendBin == 0 & RFR_cat_2019 == 3] <- 31
RFR_GRACE_combinations[GraceTrendBin == 1 & RFR_cat_2019 == 3] <- 32

RFR_GRACE_combinations[GraceTrendBin == -1 & RFR_cat_2019 == 4] <- 40
RFR_GRACE_combinations[GraceTrendBin == 0 & RFR_cat_2019 == 4] <- 41
RFR_GRACE_combinations[GraceTrendBin == 1 & RFR_cat_2019 == 4] <- 42
plot(RFR_GRACE_combinations)

# Calculate population within each combination
RFR_GRACE_combinations_Distr <- zonal(POPULATION2015, RFR_GRACE_combinations, "sum")
RFR_GRACE_combinations_Distr %<>% as.data.frame()
colnames(RFR_GRACE_combinations_Distr) <- c("ID", "POP")
head(RFR_GRACE_combinations_Distr)
b <- RFR_GRACE_combinations_Distr
b$ID <- NULL
names(b) <- NULL
b %<>% unlist() %>% as.vector()
b <- matrix(b, nrow = 5, ncol = 3, byrow = TRUE) %>% as.data.frame()
colnames(b) <- c("Pop.Drying", "Pop.Neut", "Pop.Wetting")
rownames(b) <- c("Low", "LowMed", "MedHigh", "High", "ExHigh")
b$PctDRY <- 100 * (b$Pop.Drying) / (b$Pop.Drying + b$Pop.Neut + b$Pop.Wetting)
b$PctNEU <- 100 * (b$Pop.Neut) / (b$Pop.Drying + b$Pop.Neut + b$Pop.Wetting)
b$PctWET <- 100 * (b$Pop.Wetting) / (b$Pop.Drying + b$Pop.Neut + b$Pop.Wetting)
b$TotalPop <- (b$Pop.Drying + b$Pop.Neut + b$Pop.Wetting)

# Ternary plot of stress results
# Initialize Ternary plot
tiff('C:/Users/Tom/Desktop/Ternary_FLOOD.tiff', units="in", width=7, height=7, res=300, compression = 'lzw')

TernaryPlot(alab="\u2190 % Drying", blab="\u2190 % Stable", clab="% Wetting \u2192",
            point='down', 
            # scale = 4,
            grid.lines=5, grid.lty='dotted',
            grid.minor.lines=1, grid.minor.lty='dotted')

# add data points
data_points <- list(
  L = c(b[1,1], b[1,2], b[1,3]),
  LM = c(b[2,1], b[2,2], b[2,3]),
  MH = c(b[3,1], b[3,2], b[3,3]),
  H = c(b[4,1], b[4,2], b[4,3]),
  EH = c(b[5,1], b[5,2], b[5,3])
)

alpha = 3
scaleFACTOR <- c(alpha*sqrt(a[1,7]/a[4,7]), 
                 alpha*sqrt(a[2,7]/a[4,7]), 
                 alpha*sqrt(a[3,7]/a[4,7]), 
                 alpha*sqrt(a[4,7]/a[4,7]), 
                 alpha*sqrt(a[5,7]/a[4,7]))
AddToTernary(points, data_points, pch = 21, cex = scaleFACTOR, bg = c('azure3', 
                                                                      'lightblue1', 'dodgerblue', 
                                                                      'dodgerblue3', 'dodgerblue4'))
# AddToTernary(text, data_points, names(data_points), cex=0.8, font=1)
legend('bottomleft', 
       legend=c('Low', 'LowMed', 'MedHigh', 'High', 'ExHigh'),
       cex=0.8, bty='n', pch=21, pt.cex=1.8,
       pt.bg=c('azure3', 
               'lightblue1', 'dodgerblue', 
               'dodgerblue3', 'dodgerblue4')
)

dev.off()

#####
# Combined plot
TernaryPlot(alab="\u2190 % Drying", blab="\u2190 % Stable", clab="% Wetting \u2192",
            point='down', 
            # scale = 4,
            grid.lines=5, grid.lty='dotted',
            grid.minor.lines=1, grid.minor.lty='dotted')

# add data points
data_points.FLOOD <- list(
  L = c(b[1,1], b[1,2], b[1,3]),
  LM = c(b[2,1], b[2,2], b[2,3]),
  MH = c(b[3,1], b[3,2], b[3,3]),
  H = c(b[4,1], b[4,2], b[4,3]),
  EH = c(b[5,1], b[5,2], b[5,3])
)

alpha = 3
scaleFACTOR.FLOOD <- c(alpha*sqrt(a[1,7]/a[4,7]), 
                 alpha*sqrt(a[2,7]/a[4,7]), 
                 alpha*sqrt(a[3,7]/a[4,7]), 
                 alpha*sqrt(a[4,7]/a[4,7]), 
                 alpha*sqrt(a[5,7]/a[4,7]))
AddToTernary(points, data_points.FLOOD, pch = 21, cex = scaleFACTOR, bg = c('azure3', 
                                                                      'lightblue1', 'dodgerblue', 
                                                                      'dodgerblue3', 'dodgerblue4'))

data_points.STRESS <- list(
  L = c(a[2,1], a[2,2], a[2,3]),
  LM = c(a[3,1], a[3,2], a[3,3]),
  MH = c(a[4,1], a[4,2], a[4,3]),
  H = c(a[5,1], a[5,2], a[5,3]),
  EH = c(a[6,1], a[6,2], a[6,3]),
  A = c(a[1,1], a[1,2], a[1,3])
)

alpha = 3
scaleFACTOR.STRESS <- c(alpha*sqrt(a[2,7]/a[4,7]), 
                 alpha*sqrt(a[3,7]/a[4,7]), 
                 alpha*sqrt(a[4,7]/a[4,7]), 
                 alpha*sqrt(a[5,7]/a[4,7]), 
                 alpha*sqrt(a[6,7]/a[4,7]),
                 alpha*sqrt(a[1,7]/a[4,7]))
AddToTernary(points, data_points.STRESS, pch = 21, cex = scaleFACTOR.STRESS, bg = c('lightgoldenrod1', 
                                                                      'yellow', 'orange', 
                                                                      'red', 'red4',
                                                                      'darkgray'))
# AddToTernary(text, data_points, names(data_points), cex=0.8, font=1)
legend('bottomleft', 
       legend=c('Arid', 'Low', 'LowMed', 'MedHigh', 'High', 'ExHigh'),
       cex=0.8, bty='n', pch=21, pt.cex=1.8,
       pt.bg=c('darkgray', 'lightgoldenrod1', 
               'yellow', 'orange', 
               'red', 'red4')
)




