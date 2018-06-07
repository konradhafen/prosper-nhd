
# Do setup ----------------------------------------------------------------

rm(list=ls())

setwd("E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\outputs\\csv")
fn <- "obs_hr_nhd_scpdsi.csv"

library(tidyverse)

indat <- as.data.frame(read_csv(fn))

# Functions ---------------------------------------------------------------

misclass <- function(fcode, class)
{
  if ((fcode == 46006 | fcode == 55800) & class == "Dry")
  {
    return(1)
  }
  else if ((fcode == 46007 | fcode == 46003) & class == "Wet")
  {
    return(1)
  }
  else
  {
    return(0)
  }
}

# Identify misclassifications ---------------------------------------------

indat$mc <- mapply(misclass, indat$FCODE, indat$Category)


# Model misclassifications ------------------------------------------------

logr.dif <- glm(mc ~ abs(pdsi_dif), data=indat, family="binomial")
