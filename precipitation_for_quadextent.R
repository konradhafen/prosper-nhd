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
NAvalue(pptbrick) <- 0.0
quads <- readOGR("quads", "Historical_Topo_Maps_subset")

quadmean <- extract(pptbrick, quads, fun = mean, df = T, na.rm = T)
