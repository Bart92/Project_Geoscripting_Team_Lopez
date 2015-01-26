"""Main script for our project."""

source('Functions/TextToRasterStack.R')
library("rgdal")
library("raster")

### Step 1: Downloading fluxtower data and converting it to raster stack.
dataset <- "fn_nnloobos"

NDVI <- text_to_rasterstack(dataset)

### Step 3: Preforming supervised classification, forestcover classification
coordinate_matr <- html_to_coordlist(dataset)
filelink <- paste("http://commondatastorage.googleapis.com/earthenginepartners-hansen/GFC2014/Hansen_GFC2014_treecover2000_",(coordinate_matr[1,1]%/%10)*10+10,"N_0",if(coordinate_matr[1,2]%/%10==0){print("00")} else{print((coordinate_matr[1,2]%/%10)*10)},"E.tif",sep="")
download.file(filelink, paste("forestcover_",dataset,".tif",sep=""))
forestcover <- raster(paste("forestcover_",dataset,".tif",sep=""))
sinusoidal <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
forestcover <- spTransform(forestcover, CRS(sinusoidal))
forestcover <- crop(forestcover, NDVI)
# @ Peter: als ik deze convert dan krijg ik een error message: iets met rgdal.
# Weet niet of dat aan mijn rgdal ligt of aan iets anders. (waarschijnlijk mijn rgdal....)

### Step 4: Preform B-fast analysis for forest change during the years

### Step 5: Make interactive map with googlevis or rastervis package
# TASK: convert raster stack to animated gif
for(i in 1:5){
  png(filename = sprintf("Data/NDVI_%d.png",i))
  plot(NDVI[[i]])
  dev.off()    
}

# You will have to download ImageMagick to run this.
make.mov <- function(){
  unlink("plot.mpg")
  system("convert -delay 3 NDVI_*.png NDVI.mpg")
}
setwd("/Users/Bart/Desktop/GeoScripting/FinalProjectTeamLopez/Data")
make.mov()
### @ Peter: ik krijg een .mpg file maar ik kan hem niet openen. Weet niet of dit aan mijn
# mediaplayer ding ligt of aan de file zelf.

