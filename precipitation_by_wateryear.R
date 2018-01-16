
###########################################################################
# Create multilayer precitation rasters for each water year
# Create multilayer precipitation raster for each year 1896-2016
# Data from PRISM FTP prism.nacse.org
###########################################################################

# Do setup ----------------------------------------------------------------

rm(list=ls())
pptdir = "E:/konrad/Projects/usgs/prosper-nhd/data/ppt"
setwd(pptdir)
library(raster)

# Rasters by month and year for period of record --------------------------

fnbase = "PRISM_ppt_stable_4kmM"
fnend = "_bil.bil"
totalbrick <- brick()
for (year in 1895:2015)
{
  pptbrick <- brick()
  fnmid <- ifelse(year < 1981, "2_", "3_")
  for (month in 10:12)
  {
    pptbrick <- addLayer(pptbrick, raster(paste("raw/", fnbase, fnmid, year, month, fnend, sep="")))
  }
  fnmid <- ifelse(year+1 < 1981, "2_", "3_")
  for (month in 1:9)
  {
    pptbrick <- addLayer(pptbrick, raster(paste("raw/", fnbase, fnmid, year+1, "0", month, fnend, sep="")))
  }
  
  totalbrick <- addLayer(totalbrick, calc(pptbrick, fun=sum, na.rm = T))
  writeRaster(pptbrick, paste("wateryear/ppt_", year+1, ".tif", sep=""))
  print(year+1)
}
writeRaster(totalbrick, "wateryear/ppt.tif")


