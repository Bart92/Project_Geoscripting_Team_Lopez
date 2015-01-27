# Install packages.
library("fields")
library("raster")
library("rgdal")
source('Functions/ReadingCoordsFromHTML.R')

# Write function.
text_to_rasterstack <- function(fluxtower){
  input_txt_file <- paste("http://daac.ornl.gov/data/modis_ascii_subsets/C5_MOD13Q1/data/MOD13Q1.",
                    paste(fluxtower,".txt",sep = ""), sep = "")
  download.file(input_txt_file, paste(fluxtower,".txt",sep = ""))
  coordinate_matr <- html_to_coordlist(fluxtower)
  'This function converts MODIS Fluxtower NDVI .txt dataset to a georeferenced raster stack.
  The expected input is the txt file downoaded from: http://daac.ornl.gov/modisfixedsite/.
  The filename should be exactly the same as the flux tower list'

  data <- read.csv(paste(fluxtower,".txt",sep=""))
  
  # Projections
  WGS84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  sinusoidal <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
  spatialpoints <- SpatialPoints(coordinate_matr)
  proj4string(spatialpoints) <- CRS(WGS84)
  Extent <- spTransform(spatialpoints, CRS(sinusoidal))
  
  Datelist <- as.numeric()
  NDVIlist <- c()
  for(i in 1:as.integer((length(data[,1])-5)/12)){
    n = (i-1)*12+5
    NDVI <- data[n,c(7:790)]
    NDVI <- as.matrix(t(matrix(data=as.numeric(NDVI), ncol=28,nrow=28)))
    NDVI <- raster(NDVI,extent(Extent)[1],extent(Extent)[2],extent(Extent)[3],extent(Extent)[4],CRS(sinusoidal))
    NDVIlist <- c(NDVIlist, NDVI)
    Datelist <- append(Datelist, as.Date(strptime(paste(as.numeric(substr(as.character(data$HDFname[n]),10,13)), as.numeric(substr(as.character(data$HDFname[n]),14,16))), "%Y %j")))
  }
  stackNDVI <- stack(NDVIlist)
  # TASK: check whether check.names=F helps against X in front of names.
  names(stackNDVI) <- Datelist
  return(stackNDVI)
}
describe <- function(obj) attr(obj, "help")
attr(text_to_rasterstack, "help") <- "This function converts MODIS Fluxtower NDVI .txt dataset to a georeferenced raster stack.
  The expected input is the txt file downoaded from: http://daac.ornl.gov/modisfixedsite/.
  The filename should be exactly the same as the flux tower list"
describe(text_to_rasterstack)