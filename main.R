"""Main script for our project."""

setwd('/Functions')
source('ReadingCoordsFromHTML.R')

### Step 1: Downloading fluxtower data

### Step 2: Georeferencing fluxtower .txt file data (make function)

### Step 3: Preforming supervised classification, forestcover classification

### Step 4: Preform B-fast analysis for forest change during the years

### Step 5: Make interactive map with googlevis or rastervis package
library(raster)
b <- raster('data/ndvi.img')
hist(b, 1, maxpixels=1000, plot=TRUE)
plotRGB(a, 3, 2, 1, stretch='hist')