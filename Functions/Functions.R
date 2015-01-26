### here come all the functions we will make.
library("fields")
library("raster")
library("rgdal")


coordinate_list <- c(5.792069,5.696894,5.68943,5.78448,52.2,52.2,52.14167,52.14167)
### TASK: automate this coordinates download ;)
text_to_rasterstack <- function(input_txt_file, coordinate_list){
  'This function converts MODIS Fluxtower NDVI .txt dataset to a georeferenced raster stack.
  The expected input is the txt file downoaded from: http://daac.ornl.gov/'
  #create docstrings!!!
  data <- read.csv(input_txt_file)
  #projections
  WGS84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  sinusoidal <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
  ### TASK: find out how projection changes in other regions (-- +- -+).
  Extent<- matrix(coordinate_list, nrow=4,ncol=2)
  spatialpoints <- SpatialPoints(Extent)
  proj4string(spatialpoints) <- CRS(WGS84)
  Extent <- spTransform(spatialpoints, CRS(sinusoidal))
  extent(Extent)

  NDVIlist <- c()
  for(i in 1:as.integer((length(data[,1])-5)/12)){
    n = (i-1)*12+5
    NDVI <- data[n,c(7:790)]
    NDVI <- as.matrix(t(matrix(data=as.numeric(NDVI), ncol=28,nrow=28)))
    NDVI <- raster(NDVI,extent(Extent)[1],extent(Extent)[2],extent(Extent)[3],extent(Extent)[4],CRS(sinusoidal))
    NDVIlist <- c(NDVIlist, NDVI)
  }
  return(stack(NDVIlist))
}
a = text_to_rasterstack(fluxtowerspain,coordinate_list)
# TASK: convert raster stack to animated gif