
# Do setup ----------------------------------------------------------------

rm(list=ls())

library(rgdal)

wd <- "E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\nhd\\MR"
fin <- "buf20_cat.shp"
fout <- "buf20_cat_out.shp"
lyr <- "buf20_cat"

#read shapefile (buffer polygons of MR NHD)
indat <- readOGR(wd, lyr)


# Prep data ---------------------------------------------------------------

#get attributes as data frame
indat.df <- as(indat, "data.frame")

#drop un-needed stats columns
drops <- c("c_maj", "c_mean", "c_max", "c_min")
indat.df <- indat.df[, !(names(indat.df) %in% drops)]

# Identify features where class changed -----------------------------------

#get majoirty stat for each year
maj.df <- indat.df[,grepl("_maj", names(indat.df))]

#calc min and max majority value across all years
maj.df$max <- apply(maj.df, 1, max)
maj.df$min <- apply(maj.df, 1, min)

#if contains both positive and negative values classification changed
maj.df$switch <- ifelse(maj.df$max > 0 & maj.df$min < 0, 1, 0)

#difference between min and max classification values indicates confidence
maj.df$switchdif <- ifelse(maj.df$switch, maj.df$max-maj.df$min, NA)

#proportion of years wet
maj.df$pwet <- apply(maj.df[1:13]>0, 1, sum)
maj.df$pwet <- maj.df$pwet/13.0


# Add new calculations to original data -----------------------------------

indat.df <- cbind(indat.df, maj.df[c("switch", "switchdif", "pwet")])
keep <- c("REACHCODE", "switch", "switchdif", "pwet")
merge.df <- indat.df[keep]
joined <- merge(indat, merge.df, by.x="REACHCODE", by.y="REACHCODE")
writeOGR(joined, wd, "buf20_cat_out", driver="ESRI Shapefile")
