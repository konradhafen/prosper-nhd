
# Do setup ----------------------------------------------------------------

rm(list=ls())

library(rgdal)

wd <- "E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\outputs\\shp"
outwd <- "E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\outputs\\shp"
fin <- "nhd_hr_buf20_cat_subset.shp"
fout <- "nhd_hr_buf20_cat_out.shp"
lyr <- "nhd_hr_buf20_cat_subset"


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
#maj.df[is.na(maj.df)] <- 0

#calc min and max majority value across all years
# maj.df$max <- apply(maj.df, 1, max)
# maj.df$min <- apply(maj.df, 1, min)
maj.temp <- data.frame(max = apply(maj.df, 1, max))
maj.temp$min <- apply(maj.df, 1, min)
maj.temp$ctwet <- rowSums(maj.df > 0)
maj.temp$ctdry <- rowSums(maj.df < 0)
maj.temp <- transform(maj.temp, ctmin = pmin(ctwet, ctdry))
maj.temp$ctswitch <- apply(maj.df, 1, function(x) sum(diff(sign(x)) != 0, na.rm=T))

maj.df = cbind(maj.df, maj.temp)

#if contains both positive and negative values classification changed
maj.df$switch <- ifelse(maj.df$max > 0 & maj.df$min < 0, 1, 0)

#proportion of years wet
maj.df$pwet <- apply(maj.df[1:13]>0, 1, sum)
maj.df$pwet <- maj.df$pwet/13.0


# Add new calculations to original data -----------------------------------

indat.df <- cbind(indat.df, maj.df[c("ctwet", "ctdry", "switch", "ctswitch", "pwet")])
keep <- c("ID", "ctwet", "ctdry","switch", "ctswitch", "pwet")
keep <- c("ID", "ctwet", "ctdry","switch", "ctswitch", "pwet")
merge.df <- indat.df[keep]
#joined <- merge(indat, merge.df, by.x="REACHCODE", by.y="REACHCODE")
joined <- merge(indat, merge.df, by.x="ID", by.y="ID")
#joined$disagree <- ifelse(joined$FCODE==46003, joined$ctwet/13, joined$ctdry/13)
joined.df <- as(joined, "data.frame")
writeOGR(joined, outwd, outlyr, driver="ESRI Shapefile")

###########################################################################
#Use script prosper_class_analyze.R for subsetting and analysis
###########################################################################
