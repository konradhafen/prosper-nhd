
# Do setup ----------------------------------------------------------------

rm(list=ls())

library(rgdal)
library(tidyverse)

wd <- "E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\nhd\\MR"
outwd <- "E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\outputs\\shp"
fin <- "buf20_cat.shp"
fout <- "buf20_cat_out.shp"
lyr <- "buf20_cat"
fin <- "bull_trout_habitat_buffer_epsg5070.shp"
fout <- "bt_out.shp"
lyr <- "bull_trout_habitat_buffer_epsg5070"
outlyr <- "bt_out"

#read shapefile (buffer polygons of MR NHD)
#indat <- readOGR(wd, lyr)

csvfile <- "E:/konrad/Projects/usgs/prosper-nhd/data/outputs/csv/nhd_hr_buf20_cat.csv"
outcsvfile <- "E:/konrad/Projects/usgs/prosper-nhd/data/outputs/csv/nhd_hr_buf20_cat_out.csv"
outcsv_fcode <- "E:/konrad/Projects/usgs/prosper-nhd/data/outputs/csv/nhd_hr_buf20_cat_fcode.csv"
outcsv_all <- "E:/konrad/Projects/usgs/prosper-nhd/data/outputs/csv/nhd_hr_buf20_cat_maj.csv"
indat.df <- read_csv(csvfile)

# Prep data ---------------------------------------------------------------

#get attributes as data frame
#indat.df <- as(indat, "data.frame")

#drop un-needed stats columns
drops <- c("_maj", "_mean", "_max", "_min")
indat.df <- indat.df[, !(names(indat.df) %in% drops)]

# Identify features where class changed -----------------------------------

#get majoirty stat for each year
keepcols <- grepl("_maj", names(indat.df))
keepcols[match("id", names(indat.df))] <- TRUE
maj.df <- indat.df[, keepcols]
#maj.df[is.na(maj.df)] <- 0

#calc min and max majority value across all years
# maj.df$max <- apply(maj.df, 1, max)
# maj.df$min <- apply(maj.df, 1, min)
maj.temp <- data.frame(max = apply(maj.df[, 2:ncol(maj.df)], 1, max))
maj.temp$min <- apply(maj.df[, 2:ncol(maj.df)], 1, min)
maj.temp$ctwet <- rowSums(maj.df[, 2:ncol(maj.df)] > 0)
maj.temp$ctdry <- rowSums(maj.df[, 2:ncol(maj.df)] < 0)
maj.temp <- transform(maj.temp, ctmin = pmin(ctwet, ctdry))
maj.temp$ctswitch <- apply(maj.df[, 2:ncol(maj.df)], 1, function(x) sum(diff(sign(x)) != 0, na.rm=T))

maj.df = cbind(maj.df, maj.temp)

#if contains both positive and negative values classification changed
maj.df$switch <- ifelse(maj.df[, 2:ncol(maj.df)]$max > 0 & maj.df[, 2:ncol(maj.df)]$min < 0, 1, 0)

#difference between min and max classification values indicates confidence
#maj.df$switchdif <- ifelse(maj.df$switch, maj.df$max-maj.df$min, NA)

#proportion of years wet
maj.df$pwet <- apply(maj.df[2:14]>0, 1, sum)
maj.df$pwet <- maj.df$pwet/13.0


# Add new calculations to original data -----------------------------------

indat.df <- cbind(indat.df, maj.df[c("ctwet", "ctdry", "switch", "ctswitch", "pwet")])
keep <- c("ID", "ctwet", "ctdry","switch", "ctswitch", "pwet")
merge.df <- indat.df[keep]
keep <- c("ID", "FCODE", "ctwet", "ctdry","switch", "ctswitch", "pwet")
fcode.df <- indat.df[keep]
write_csv(merge.df, outcsvfile)
write_csv(fcode.df, outcsv_fcode)
keep <-append(names(maj.df), "FCODE")
remove <- c("min", "max", "ctmin")
keep <- keep[! keep %in% remove]
maj.out <- indat.df[keep]
write_csv(maj.out, outcsv_all)
#joined <- merge(indat, merge.df, by.x="REACHCODE", by.y="REACHCODE")

# Join to shapefile -------------------------------------------------------

joined <- merge(indat, merge.df, by.x="ORIG_FID", by.y="ORIG_FID")
#joined$disagree <- ifelse(joined$FCODE==46003, joined$ctwet/13, joined$ctdry/13)
joined.df <- as(joined, "data.frame")
writeOGR(joined, outwd, outlyr, driver="ESRI Shapefile")

###########################################################################
#Use script prosper_class_analyze.R for subsetting and analysis
###########################################################################
