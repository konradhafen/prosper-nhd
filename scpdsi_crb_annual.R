# Do setup ----------------------------------------------------------------

rm(list=ls())

library(raster)
library(rgdal)
library(reshape2)

setwd("E:\\konrad\\projects\\usgs\\prosper-nhd\\data\\scpdsi\\netcdf")
fn <- "scpdsi_wymean.nc"
rasdat <- brick(fn)

crb_shp <- readOGR("C:\\konrad\\USGS\\PROSPER\\data\\shp", "US_17_boundary_dissolved_wgs84")


# Functions ---------------------------------------------------------------

zonal_stats_mean <- function(rasterdata, zone)
{
  print(band)
  value <- extract(rasterdata, zone, fun=mean)
  print(value)
  return(value)
}

# Get zonal stats ---------------------------------------------------------

#setup dataframe
df <- data.frame(year=2004:2016)
df$band <- df$year - 1895

df$scpdsi <- extract(subset(rasdat, df$band), crb_shp, fun=mean)[1,]

fnout = "E:\\konrad\\projects\\usgs\\prosper-nhd\\data\\scpdsi\\csv\\scpdsi_crb_prosper.csv"
write_csv(df, fnout)
