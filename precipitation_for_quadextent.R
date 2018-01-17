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

if(!file.exists("ppt/wateryear/pptmean.tif"))
{
  pptmean <- calc(pptbrick, fun=mean, na.rm=T)
  writeRaster(pptmean, "ppt/wateryear/pptmean.tif")
}else
{
  pptmean <- raster("ppt/wateryear/pptmean.tif")
}

if(!file.exists("ppt/wateryear/pptsd.tif"))
{
  pptsd <- calc(pptbrick, fun=sd, na.rm=T)
  writeRaster(pptsd, "ppt/wateryear/pptsd.tif")
}else
{
  pptmean <- raster("ppt/wateryear/pptsd.tif")
}


# Load quad polygons ------------------------------------------------------

quads <- readOGR("quads", "Historical_Topo_Maps_subset")


# Zonal stats for each quad -----------------------------------------------

quadmean <- extract(pptmean, quads, fun = mean, df = T, na.rm = T)
quadsd <- extract(pptsd, quads, fun = sd, df = T, na.rm = T)
