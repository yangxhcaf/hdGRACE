library(ggplot2)
library(raster)
library(dplyr) 
library(rgdal) 
library(magrittr) 
library(sp)  
library(e1071) 
library(reshape2) 

## Import emerging trend, population, and water stress GeoTIFFs created in PopulationDistribution.R
EmergingTrend <- raster("E:/! GIS_files/R_gis_exports/GRACE_0d05.tif")
Pop.2015 <- raster("E:/! GIS_files/R_gis_exports/POP_2015_0d05_V1.tif")
WaterStress <- raster("E:/! GIS_files/Aqueduct/WaterStress_WRI.tif")

# generate population *times* emerging trend raster
Pop.X.trend <- raster(EmergingTrend)
Pop.X.trend <- EmergingTrend*Pop.2015

# Pull population times emerging trend total in each water stress class
Pop.x.trend.LOW     <- Pop.X.trend[WaterStress == 1] %>% as.data.frame() %>% pull(.) %>% sum(na.rm = TRUE)
Pop.x.trend.LOWMED  <- Pop.X.trend[WaterStress == 2] %>% as.data.frame() %>% pull(.) %>% sum(na.rm = TRUE)
Pop.x.trend.MEDHIGH <- Pop.X.trend[WaterStress == 3] %>% as.data.frame() %>% pull(.) %>% sum(na.rm = TRUE)
Pop.x.trend.HIGH    <- Pop.X.trend[WaterStress == 4] %>% as.data.frame() %>% pull(.) %>% sum(na.rm = TRUE)
Pop.x.trend.EXHIGH  <- Pop.X.trend[WaterStress == 5] %>% as.data.frame() %>% pull(.) %>% sum(na.rm = TRUE)
Pop.x.trend.ARID    <- Pop.X.trend[WaterStress == 0] %>% as.data.frame() %>% pull(.) %>% sum(na.rm = TRUE)

# Pull population in each water stress class
Pop.LOW     <- Pop.2015[WaterStress == 1] %>% as.data.frame() %>% pull(.) %>% sum(na.rm = TRUE)
Pop.LOWMED  <- Pop.2015[WaterStress == 2] %>% as.data.frame() %>% pull(.) %>% sum(na.rm = TRUE)
Pop.MEDHIGH <- Pop.2015[WaterStress == 3] %>% as.data.frame() %>% pull(.) %>% sum(na.rm = TRUE)
Pop.HIGH    <- Pop.2015[WaterStress == 4] %>% as.data.frame() %>% pull(.) %>% sum(na.rm = TRUE)
Pop.EXHIGH  <- Pop.2015[WaterStress == 5] %>% as.data.frame() %>% pull(.) %>% sum(na.rm = TRUE)
Pop.ARID    <- Pop.2015[WaterStress == 0] %>% as.data.frame() %>% pull(.) %>% sum(na.rm = TRUE)

# Divide population times emerging trend by population in each water stress class to get weighted average
LOW_weighted      <- Pop.x.trend.LOW/Pop.LOW
LOWMED_weighted   <- Pop.x.trend.LOWMED/Pop.LOWMED
MEDHIGH_weighted  <- Pop.x.trend.MEDHIGH/Pop.MEDHIGH
HIGH_weighted     <- Pop.x.trend.HIGH/Pop.HIGH
EXHIGH_weighted   <- Pop.x.trend.EXHIGH/Pop.EXHIGH
ARID_weighted     <- Pop.x.trend.ARID/Pop.ARID

# merge into a dataframe for plotting
Class <- c(1, 2, 3, 4, 5)
Weighted <- c(LOW_weighted, LOWMED_weighted, MEDHIGH_weighted, HIGH_weighted, EXHIGH_weighted)
Population <- c(Pop.LOW, Pop.LOWMED, Pop.MEDHIGH, Pop.HIGH, Pop.EXHIGH)
table <- data.frame(Class, Weighted, Population)

# set theme for plot
theme = theme_light() +
        theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(alpha(colour = "black", 0.3), linetype = "dashed", size = 0.25), 
        axis.title.x = element_text(size = 11, color = "black", face = "bold"),
        panel.background = element_rect(fill = alpha("black", 0.03)),
        panel.grid.major.x = element_line(alpha(colour = "black", 0.3), linetype = "dashed", size = 0.25),
        panel.grid.minor.x = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.y = element_text(color = "black", vjust = 2.5, face = "bold"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1.5),
        legend.title = element_blank(),
        legend.position = "none") 

# plot results
p <- ggplot() + 
  theme + 
  geom_hline(yintercept = 0, size = 1.5) + 
  stat_smooth(data = table, aes(x = Class, y = Weighted, colour = ..y..), lwd = 2, method = "lm", formula = y ~ poly(x,4), se = FALSE) +
  scale_colour_gradient2(low = "red 3", mid = "lemonchiffon2", high = "royalblue4",
                         midpoint = -0.15, guide = FALSE)+ 
  geom_point(data = table, aes(x = Class, y = Weighted, size = Population, fill = ..y..), pch = 21, stroke = 1.5) + 
  geom_point(data = Arid, aes(x = 1.5, y = ARID_weighted, size = Pop.ARID), fill = "black" , pch = 21, stroke = 1.5) +
  scale_fill_gradient2(low = "red 3", mid = "lemonchiffon2", high = "royalblue2",
                       midpoint = -0.15, guide = FALSE)+ 
  scale_radius("Population (millions)", range = c(2,15)) +  
  scale_x_continuous(labels = c("1" = "Low", "2" = "Low to Med.", "3" = "Med. to High", "4" = "High", "5" = "Extremely High")) +
  scale_y_continuous(limits = c(-1, 0.5), breaks = c(-1, -0.75, - 0.5, -0.25, 0, 0.25, 0.5), expand = c(0,0)) + 
  xlab("Baseline Water Stress Class") +
  ylab(expression(atop(bold('Population Weighted Mean Terrestrial'), paste(bold('Water Storage Rate of Change'~(cmy^-1)))))) + 
  annotate("text", x = 1.35, y = -0.45, label = "Arid/\nLow water use", size = 4, fontface = 2) +
  coord_flip()
p

