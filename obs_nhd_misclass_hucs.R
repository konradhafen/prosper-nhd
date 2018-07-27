# Do setup ----------------------------------------------------------------

rm(list=ls())
options(scipen=999)

setwd("E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\outputs\\csv")

#contains info from points where quad data a climate data exist
fn <- "obs_hr_nhd_scpdsi.csv"

library(tidyverse)
#library(reshape2)
#library(plyr)

indat <- as.data.frame(read_csv("misclassifications_obs_nhd_climate.csv"))
indat$huc8 <- substr(indat$HUC10, 1, 8) 


# Identify disagreements --------------------------------------------------

indat$mc <- mapply(misclass, indat$FCODE, indat$Category)
indat$nhdclass <- mapply(nhdclass, indat$FCODE)
indat$wet <- ifelse(indat$nhdclass=="Permanent", 1, 0)
indat$mctype <- mapply(misclass_type, indat$nhdclass, indat$Category)


# Examine HUC -------------------------------------------------------------

huc <- 17040220
testdat <- indat[indat$huc8 == huc,]
testdat.mcdry <- testdat[testdat$mctype=="NHD wet Observation dry",]
testdat.mcwet <- testdat[testdat$mctype=="NHD dry Observation wet",]
