rm(list=ls())

setwd("/Users/Bart/Desktop/GeoScripting/FinalProjectTeamLopez/Data")

fluxtowerspain <- c("fn_estoledo.txt")
filenamespain <- paste("ftp://daac.ornl.gov/data/modis_ascii_subsets/C5_MOD13Q1/data/MOD13Q1.",
                       fluxtowerspain, sep = "")
download.file(filenamespain, fluxtowerspain)

modis <- read.csv(fluxtowerspain)
names(modis)

E=modis[5,c(7:790)]
str(as.numeric(E))
G=matrix(data=as.numeric(E), ncol=28,nrow=28)
GG=t(G)
GG=as.matrix(GG)

#K <- raster(G)
#plot(K)

library("fields")
library("sp")
library(raster)
library("rgdal")

image.plot(GG)

# projectRaster()

## the site coordinates are projected in lat/long
latlong <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
## convert these to sinusoidal projection
modissinprojection <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
## and then define your modis spatial point data frame
resolution <- 250
## define the extent by looking at the subset site information (xmin, xmax, ymin,ymax)
# 231

AA <- matrix(c(5.792069,5.696894,5.68943,5.78448,52.2,52.2,52.14167,52.14167), nrow=4,ncol=2)
spatialpoints <- SpatialPoints(AA)
proj4string(spatialpoints) <- CRS(latlong)
AAA <- spTransform(spatialpoints, CRS(modissinprojection))
plot(AAA)
extent(AAA)[1]


AG <- raster(GG,388256,394742.4,5797896,5804382,CRS(modissinprojection))
plot(AG)

