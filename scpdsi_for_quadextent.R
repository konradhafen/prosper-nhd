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

if(!file.exists("scpdsi/csv/scpdsi_wysd.csv"))
{
  quadsd <- extract(scpdsisd, quads, fun = mean, df = T, na.rm = T)
  colnames(quadsd) <- c("ID", "pdsi_sd")
  write.csv(quadsd, "scpdsi/csv/scpdsi_wysd.csv", row.names = F)
}else
{
  quadsd <- read.csv("scpdsi/csv/scpdsi_wysd.csv")
}

scpdsistat <- merge(quadmean, quadsd, by="ID")
scpdsistat$testID <- scpdsistat$ID
scpdsistat$joinID <- NA

# Get precip in quad for field check year ---------------------------------

scpdsiall = brick("scpdsi/wateryear/scpdsi_wymean.tif")
scpdsistat$pdsi_check <- NA
scpdsistat$year_chks <- NA
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
    scpdsistat[i, "pdsi_check"] <- extract(subset(scpdsiall,layer), quads[i,], fun = mean)
    scpdsistat[i, "year_chks"] <- year
    scpdsistat[i, "joinID"] <- as.integer(as.character(data.frame(quads)[i, "Join_ID"]))
  }else
  {
    print(paste(layer, year))
  }
  
  
  if(i%%1000 == 0)
  {
    print(paste(i, "of", nrow(scpdsistat)))
  }
}

scpdsistat$pdsi_per <- pnorm((scpdsistat$pdsi_check - scpdsistat$pdsi_mean) / scpdsistat$pdsi_sd)*100
write.csv(scpdsistat, "scpdsi/csv/scpdsi_wy_all.csv", row.names = F)

# Join precip mean and sd to shapefile ------------------------------------

join1 <- merge(quads, scpdsistat, by.x="Join_ID", by.y="joinID")
#join2 <- merge(join1, quadsd, by.x="US_7_ID", by.y="ID")
writeOGR(join1, "quads", "quads_ppt_scpdsi", driver="ESRI Shapefile", overwrite_layer=T)


# Add percentiles --- OLD CODE --------------------------------------------

quads <- readOGR("quads", "quads_ppt_scpdsi")
quads$ppt_per <- pnorm((quads$ppt_check - quads$ppt_mean) / quads$ppt_sd)*100
quads$pdsi_per <- pnorm((quads$pdsi_check - quads$pdsi_mean) / quads$pdsi_sd)*100



