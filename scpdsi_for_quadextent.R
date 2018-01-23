###########################################################################
# Calculate zonal stats for each quad extent
# Run after precipitation for quads to create shp with scpdsi and ppt data
###########################################################################

# Do setup ----------------------------------------------------------------
rm(list=ls())
datadir = "E:/konrad/Projects/usgs/prosper-nhd/data"
subdir = "scpdsi/wateryear"
fn1 = "scpdsi_wyOverallMean.tif"
fn2 = "scpdsi_wyOverallSD.tif"
setwd(datadir)
library(raster)
library(rgdal)


# Load rasters ------------------------------------------------------------

scpdsimean <- raster(paste(subdir, "/", fn1, sep=""))
scpdsisd <- raster(paste(subdir, "/", fn2, sep=""))

# Load quad polygons ------------------------------------------------------

quads <- readOGR("quads", "quads_ppt") #path to joined shp resulting from file 'precipitation_for_quadextent.R'


# Zonal stats for each quad -----------------------------------------------

if(!file.exists("scpdsi/csv/scpdsi_wymean.csv"))
{
  quadmean <- extract(scpdsimean, quads, fun = mean, df = T, na.rm = T)
  colnames(quadmean) <- c("ID", "pdsi_mean")
  write.csv(quadmean, "scpdsi/csv/scpdsi_wymean.csv", row.names = F)
}else
{
  quadmean <- read.csv("scpdsi/csv/scpdsi_wymean.csv")
}

if(!file.exists("scpdsi/csv/scpdsi_wymean.csv"))
{
  quadsd <- extract(scpdsisd, quads, fun = sd, df = T, na.rm = T)
  colnames(quadsd) <- c("ID", "pdsi_sd")
  write.csv(quadsd, "scpdsi/csv/scpdsi_wysd.csv", row.names = F)
}else
{
  quadsd <- read.csv("scpdsi/csv/scpdsi_wysd.csv")
}

scpdsistat <- merge(quadmean, quadsd, by="ID")

# Get precip in quad for field check year ---------------------------------

scpdsiall = brick("scpdsi/wateryear/scpdsi_wymean.tif")
scpdsistat$ppt_check <- NA
for (i in 1:nrow(scpdsistat))
{
  year <- as.integer(as.character(data.frame(quads)[i, "Field_Chec"]))
  if (year == 0)
  {
    year <- as.integer(as.character(data.frame(quads)[i, "Survey_Yea"]))
  }
  layer <- year - 1895
  if (layer > 0 & layer < 122)
  {
    scpdsistat[i, "ppt_check"] <- extract(subset(scpdsiall,layer), quads[i,], fun = mean)
  }else
  {
    print(paste(layer, year))
  }
  
  
  if(i%%1000 == 0)
  {
    print(paste(i, "of", nrow(scpdsistat)))
  }
}

# Join precip mean and sd to shapefile ------------------------------------

join1 <- merge(quads, quadmean, by.x="US_7_ID", by.y="ID")
join2 <- merge(join1, quadsd, by.x="US_7_ID", by.y="ID")
writeOGR(join2, "quads", "quads_ppt_scpdsi", driver="ESRI Shapefile")



