rm(list=ls())

install.packages("strucchange")
install.packages("forecast")
install.packages("zoo")
install.packages("bfast", repos = "http://R-Forge.R-project.org", dependencies = TRUE)

setwd("M:/My Documents/2013/GRS32306/exercise 3 R scripting")

# ## a function to create a regular 'ts' (time series) object in R using time information (dt)
timeser <- function(index, dt) {
  z <- zoo(index, dt)
  yr <- as.numeric(format(time(z), "%Y"))
  jul <- as.numeric(format(time(z), "%j"))
  delta <- min(unlist(tapply(jul, yr, diff))) # 16
  zz <- aggregate(z, yr + (jul - 1)/delta/23)
  (tso <- as.ts(zz))
  return(tso)
}
# ## a function to remove values (set NA) that do not equal a certain criteria
sel <- function(y, crit) {
  ts.sel <- y
  ts.sel[!crit] <- NA
  return(ts.sel)
}  
  getwd() ## the file is downloaded to your working directory
  
  fluxtower <- c("fn_nlloobos.txt")
  filename <- paste("ftp://daac.ornl.gov//data/modis_ascii_subsets//C5_MOD13Q1/data/MOD13Q1.",
                    fluxtower, sep = "")
#   ## if the file exists already in your working directory nothing will happen:
  if (!file.exists(fluxtower)) {
    download.file(filename, fluxtower)
    modis <- read.csv(fluxtower, colClasses = "character")
  } else {
    modis <- read.csv(fluxtower, colClasses = "character")
  }
#   ## if the above step does not work you can download the data manually (go to 'Manual
#   ## Downloading').

fluxtower2 <- c("fn_autumbar.txt")

modis <- read.csv(fluxtower)
modis2 <- read.csv(fluxtower2)

ndvibandname <- modis$Band[5]
rel <- modis$Band[7]

j <- (436) + 6 # we are adding 6 since the first data column is the 7th column
reliability <- as.numeric(modis[modis$Band == rel, j]) # reliability data
NDVI <- as.numeric(modis[modis$Band == ndvibandname, j]) # NDVI data
DATUM <- modis[modis$Band == ndvibandname, 3] # dates
DATUM <- as.Date(DATUM, "A%Y%j") # convert to a datum type

library(zoo) ## load the package
##
## Attaching package: 'zoo'
##
## The following objects are masked from 'package:base':
##
## as.Date, as.Date.numeric
ts.rel <- timeser(reliability, DATUM)
ts.NDVI <- timeser(NDVI/10000, DATUM)

plot(ts.NDVI, ylab = "NDVI")


## this is an example for two pixels try it out and customize for your own needs
j <- 100:105
t <- modis[modis$Band == ndvibandname, j] # extract NDVI data
tt <- data.matrix(t)/10000 ## convert to a data matrix and divide by 10000
ttt <- ts(apply(tt, 2, timeser, DATUM), start = c(2000, 4), freq = 23)
## convert to a regular time series object plot(ttt) ## plot all the time series derive the
## statistics (max, mean):
maxt <- ts(apply(ttt, 1, max, na.rm = TRUE), start = c(2000, 4), freq = 23)
meant <- ts(apply(ttt, 1, mean, na.rm = TRUE), start = c(2000, 4), freq = 23)
medt <- ts(apply(ttt, 1, median, na.rm = TRUE), start = c(2000, 4), freq = 23)
## plot
plot(maxt, col = "green", ylim = c(0, 1))
lines(medt, col = "red")
##

## this is an example for two pixels try it out and customize for your own needs
i <- 400:405
ti <- modis[modis$Band == ndvibandname, i] # extract NDVI data
tti <- data.matrix(ti)/10000 ## convert to a data matrix and divide by 10000
ttti <- ts(apply(tti, 2, timeser, DATUM), start = c(2000, 4), freq = 23)
## convert to a regular time series object plot(ttt) ## plot all the time series derive the
## statistics (max, mean):
maxti <- ts(apply(ttti, 1, max, na.rm = TRUE), start = c(2000, 4), freq = 23)
meanti <- ts(apply(ttti, 1, mean, na.rm = TRUE), start = c(2000, 4), freq = 23)
medti <- ts(apply(ttti, 1, median, na.rm = TRUE), start = c(2000, 4), freq = 23)
## plot
plot(maxti, col = "green", ylim = c(0, 1))
lines(medti, col = "red")
##

## this is an example for two pixels try it out and customize for your own needs
k <- 700:705
tk <- modis[modis$Band == ndvibandname, k] # extract NDVI data
ttk <- data.matrix(tk)/10000 ## convert to a data matrix and divide by 10000
tttk <- ts(apply(ttk, 2, timeser, DATUM), start = c(2000, 4), freq = 23)
## convert to a regular time series object plot(ttt) ## plot all the time series derive the
## statistics (max, mean):
maxtk <- ts(apply(tttk, 1, max, na.rm = TRUE), start = c(2000, 4), freq = 23)
meantk <- ts(apply(tttk, 1, mean, na.rm = TRUE), start = c(2000, 4), freq = 23)
medtk <- ts(apply(tttk, 1, median, na.rm = TRUE), start = c(2000, 4), freq = 23)
## plot
plot(maxtk, col = "green", ylim = c(0, 1))
lines(medtk, col = "red")
##

plot(ts.NDVI, col = "black", ylab = "NDVI", ylim = c(0, 1), 
     main="NDVI comparison: overall vs. groups of pixels")
lines(medt, col = "700")
lines(medti, col = "467")
lines(medtk, col = "red")
legend(2000,0.2,("NDVI, medt, medti, medtk"),lty=c(1,1,1,1),lwd=c(2.5,2.5,2.5,2.5),col=c("black","288","467","red"))


## this is an example for two pixels try it out and customize for your own needs
klm <- c(407:409,435:437,463:465)
tklm <- modis[modis$Band == ndvibandname, klm] # extract NDVI data
ttklm <- data.matrix(tklm)/10000 ## convert to a data matrix and divide by 10000
tttklm <- ts(apply(ttklm, 2, timeser, DATUM), start = c(2000, 4), freq = 23)
## convert to a regular time series object plot(ttt) ## plot all the time series derive the
## statistics (max, mean):
maxtklm <- ts(apply(tttklm, 1, max, na.rm = TRUE), start = c(2000, 4), freq = 23)
meantklm <- ts(apply(tttklm, 1, mean, na.rm = TRUE), start = c(2000, 4), freq = 23)
medtklm <- ts(apply(tttklm, 1, median, na.rm = TRUE), start = c(2000, 4), freq = 23)
klmspecial <- if(meantklm > 0.4) meantklm else NA

klmspecial <- meantklm
klmspecial[meantklm <= 0.4] <- NA

## plot
plot(klmspecial, col = "blue", ylim = c(0, 1))
lines(ts.NDVI, col = "red")
##

plot(ts.NDVI)
lines(sel(ts.NDVI, ts.rel > 1), col = "red", type = "p")
legend("bottomleft", "pixels with a low reliablity", col = 2, pch = 1)

ts.clNDVI <- ts.NDVI
ts.clNDVI[ts.rel > 1] <- NA # delete data with reliability > 1

plot(ts.NDVI, col = "red")
lines(ts.clNDVI, lwd = 2, col = "black")

## install these package in case they are not available on your computer
install.packages("strucchange")
install.packages("forecast")
install.packages("bfast", repos = "http://R-Forge.R-project.org")

library("bfast")

ts.clNDVIfilled <- na.approx(ts.clNDVI)
plot(ts.clNDVIfilled, ylim = c(0.1, 1))

rdist <- 25/length(ts.clNDVIfilled)
## ratio of distance between breaks (time steps) and length of the time series
fit <- bfast(ts.clNDVIfilled, h = rdist, season = "harmonic", max.iter = 1)
plot(fit, main = "BFAST analysis")

help(bfast)
## for more info try out the examples in the bfast help section!
plot(harvest, ylab = "NDVI") # MODIS 16-day cleaned and interpolated NDVI time series
(rdist <- 10/length(harvest))
# ratio of distance between breaks (time steps) and length of the time series
fit <- bfast(harvest, h = rdist, season = "harmonic", max.iter = 1, breaks = 2)
plot(fit)
## plot anova and slope of the trend identified trend segments
plot(fit, main = "")

# ----------------------SPAIN---------------------------------------
#-------------------------------------------------------------------------------
# ----------------------------------------------------------------------
fluxtowerspain <- c("fn_estoledo.txt")
filenamespain <- paste("ftp://daac.ornl.gov/data/modis_ascii_subsets/C5_MOD13Q1/data/MOD13Q1.",
                  fluxtowerspain, sep = "")
#   ## if the file exists already in your working directory nothing will happen:
if (!file.exists(fluxtowerspain)) {
  download.file(filenamespain, fluxtowerspain)
}

modisspain <- read.csv(fluxtowerspain)

relspain <- modisspain$Band[7]

ndvibandnamespain <- modisspain$Band[5]
jspain <- (436) + 6 # we are adding 6 since the first data column is the 7th column
reliabilityspain <- as.numeric(modisspain[modisspain$Band == relspain, jspain]) # reliability data
NDVIspain <- as.numeric(modisspain[modisspain$Band == ndvibandnamespain, jspain]) # NDVI data
DATUMspain <- modisspain[modisspain$Band == ndvibandnamespain, 3] # dates
DATUMspain <- as.Date(DATUMspain, "A%Y%j") # convert to a datum type

ts.relspain <- timeser(reliabilityspain, DATUMspain)
ts.NDVIspain <- timeser(NDVIspain/10000, DATUMspain)
ts.clNDVIspain <- ts.NDVIspain
ts.clNDVIspain[ts.relspain > 1] <- NA # delete data with reliability > 1


ts.clNDVIfilledspain <- na.approx(ts.clNDVIspain)
plot(ts.clNDVIfilledspain, ylim = c(0.1, 1))

rdistspain <- 25/length(ts.clNDVIfilledspain)
## ratio of distance between breaks (time steps) and length of the time series
fitspain <- bfast(ts.clNDVIfilledspain, h = rdistspain, season = "harmonic", max.iter = 1)
plot(fitspain, main = "BFAST analysis")

# ----------------------------------------------------
# monspain <- bfastmonitor(ts.clNDVIfilledspain, start = c(2013, 23), formula = response ~ harmon + trend,
#                          history = c("BP"), order = 4)
# plot(monspain, main = "bfastmonitor results")

monspain <- bfastmonitor(ts.clNDVIfilledspain, start = c(2011, 23), formula = response ~ trend,
                    history = c(2005,1))
plot(monspain, main = "bfastmonitor results")

