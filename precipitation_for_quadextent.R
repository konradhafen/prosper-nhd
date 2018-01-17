###########################################################################
# Calculate mean annual precipitation for each quad extent
# Calculate standard deviation of annual precipitation for each quad extent
###########################################################################

# Do setup ----------------------------------------------------------------
rm(list=ls())
datadir = "E:/konrad/Projects/usgs/prosper-nhd/data"
setwd(datadir)
library(raster)
library(rgdal)


# Load precip raster ------------------------------------------------------

pptbrick <- brick("ppt/wateryear/ppt.tif")
NAvalue(pptbrick) <- 0.0 #set 0.0 to NA

# Calculate mean and sd and save new rasters ------------------------------

pptmean <- calc(pptbrick, fun=mean, na.rm=T)
pptsd <- calc(pptbrick, fun=sd, na.rm=T)
writeRaster(pptmean, "ppt/wateryear/pptmean.tif")
writeRaster(pptsd, "ppt/wateryear/pptsd.tif")

# Load quad polygons ------------------------------------------------------

quads <- readOGR("quads", "Historical_Topo_Maps_subset")

quadmean <- extract(pptbrick, quads, fun = mean, df = T, na.rm = T)
quadsd <- extract(pptbrick, quads, fun = sd, df = T, na.rm = T)
