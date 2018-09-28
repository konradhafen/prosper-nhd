
# Load data ---------------------------------------------------------------

rm(list=ls())

setwd("E:/konrad/Projects/usgs/prosper-nhd/data/outputs/csv")
fn <- "huc12.csv"

indat <- read.csv(fn)
fn <- "huc12_strord_mr.csv"
indat.str <- read.csv(fn)


# Identify headwater HUC12s -----------------------------------------------

nothw <- unique(indat$TOHUC)
indat$hw <- ifelse(!indat$HUC12 %in% nothw, 1, 0)
hw <- indat[,c("HUC12", "hw")]
hw$HUC12 <- as.character(hw$HUC12)


# Save csv ----------------------------------------------------------------

write.csv(hw, "E:/konrad/Projects/usgs/prosper-nhd/data/nhd/WBD/huc12_headwater.csv", row.names=F)


# Observations in huc12s --------------------------------------------------

obsdat <- read.csv("obs_misclass_huc12.csv")
obsdat <- obsdat[!(obsdat$Category=="Wet" & obsdat$Month<8),]
table(obsdat$hw, obsdat$mctype)


# Disagreement by stream order --------------------------------------------

table(indat.str$mc, indat.str$StreamOrde)
table(indat.str$Category, indat.str$StreamOrde)
table(indat.str$mctype, indat.str$StreamOrde)


# Stream order by nhdclass ------------------------------------------------

indat.str <- indat.str[indat.str$nhdclass=="Intermittent" | indat.str$nhdclass=="Perennial",]
indat.str <- indat.str[!(indat.str$Category=="Wet" & indat.str$Month<8),]

ggplot(indat.str, aes(x=StreamOrde)) + 
  geom_histogram(binwidth = 1) + 
  facet_wrap(~nhdclass, ncol=1)
