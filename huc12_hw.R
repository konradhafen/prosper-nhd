
# Load data ---------------------------------------------------------------

rm(list=ls())

setwd("E:/konrad/Projects/usgs/prosper-nhd/data/outputs/csv")
fn = "huc12.csv"

indat = read.csv(fn)


# Identify headwater HUC12s -----------------------------------------------

nothw <- unique(indat$TOHUC)
indat$hw <- ifelse(!indat$HUC12 %in% nothw, 1, 0)
hw <- indat[,c("HUC12", "hw")]
hw$HUC12 <- as.character(hw$HUC12)


# Save csv ----------------------------------------------------------------

write.csv(hw, "E:/konrad/Projects/usgs/prosper-nhd/data/nhd/WBD/huc12_headwater.csv", row.names=F)


# Observations in huc12s --------------------------------------------------

obsdat <- read.csv("obs_huc12.csv")
table(obsdat$hw, obsdat$Category)
