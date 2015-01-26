rm(list=ls())

setwd("M:/My Documents/2013/GRS32306/exercise 3 R scripting")

D=read.csv("modissubsetinfo.csv")

D[427,]

fluxtowerspain <- c("fn_estoledo.txt")
filenamespain <- paste("ftp://daac.ornl.gov/data/modis_ascii_subsets/C5_MOD13Q1/data/MOD13Q1.",
                       fluxtowerspain, sep = "")
  download.file(filenamespain, fluxtowerspain)

modis <- read.csv(fluxtower)
names(modis)

E=modis[5,c(7:790)]
str(as.numeric(E))
G=matrix(data=as.numeric(E), ncol=28,nrow=28)
GG=t(G)
GG=as.matrix(GG)

library("fields")
library("sp")
library(raster)
install.packages("rgdal")
library("rgdal")

image.plot(GG)

# projectRaster()

## the site coordinates are projected in lat/long
latlong <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
## convert these to sinusoidal projection
modissinprojection <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
## define the extent by looking at the subset site information (xmin, xmax, ymin,ymax)
# 231

AA <- matrix(c(D[427,7],D[427,6],D[427,9],D[427,8],D[427,11],D[427,10],D[427,13],D[427,12]), nrow=4,ncol=2)
spatialpoints <- SpatialPoints(AA)
proj4string(spatialpoints) <- CRS(latlong)
AAA <- spTransform(spatialpoints, CRS(modissinprojection))
plot(AAA)
extent(AAA)

AG <- raster(GG,xmn=-451791.5,xmx=3406266,ymn=-452506.3,ymx=4429270,CRS(modissinprojection))
plot(AG)
AG
