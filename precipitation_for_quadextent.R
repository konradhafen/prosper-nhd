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

if(!file.exists("ppt/csv/ppt_wymean.csv"))
{
  quadmean <- extract(pptmean, quads, fun = mean, df = T, na.rm = T)
  colnames(quadmean) <- c("ID", "ppt_mean")
  write.csv(quadmean, "ppt/csv/ppt_wymean.csv", row.names = F)
}else
{
  quadmean <- read.csv("ppt/csv/ppt_wymean.csv")
}

if(!file.exists("ppt/csv/ppt_wymean.csv"))
{
  quadsd <- extract(pptsd, quads, fun = sd, df = T, na.rm = T)
  colnames(quadsd) <- c("ID", "ppt_sd")
  write.csv(quadsd, "ppt/csv/ppt_wysd.csv", row.names = F)
}else
{
  quadsd <- read.csv("ppt/csv/ppt_wysd.csv")
}

pptstat <- merge(quadmean, quadsd, by="ID")

# Get precip in quad for field check year ---------------------------------

pptall = brick("ppt/wateryear/ppt.tif")
pptstat$ppt_check <- NA
for (i in 1:nrow(pptstat))
{
  year <- as.integer(as.character(data.frame(quads)[i, "Field_Chec"]))
  if (year == 0)
  {
    year <- as.integer(as.character(data.frame(quads)[i, "Survey_Yea"]))
  }
  layer <- year - 1895
  pptstat[i, "ppt_check"] <- extract(subset(pptall,layer), quads[i,], fun = mean)
  
  if(i%%1000 == 0)
  {
    print(paste(i, "of", nrow(pptstat)))
  }
}

# Join precip mean and sd to shapefile ------------------------------------

join1 <- merge(quads, quadmean, by.x="US_7_ID", by.y="ID")
join2 <- merge(join1, quadsd, by.x="US_7_ID", by.y="ID")
writeOGR(join2, "quads", "quads_ppt", driver="ESRI Shapefile")



