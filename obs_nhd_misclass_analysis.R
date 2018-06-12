
# Do setup ----------------------------------------------------------------

rm(list=ls())

setwd("E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\outputs\\csv")
fn <- "obs_hr_nhd_scpdsi.csv"

library(tidyverse)

indat <- as.data.frame(read_csv(fn))
indat <- indat[indat$ppt_pt > 0,]

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

nhdclass <- function(fcode)
{
  if (fcode == 46006 | fcode == 55800)
  {
    return('Wet')
  }
  else
  {
    return('Dry')
  }
}

# Identify misclassifications ---------------------------------------------

indat$mc <- mapply(misclass, indat$FCODE, indat$Category)
indat$nhdclass <- mapply(nhdclass, indat$FCODE)



# Correlation -------------------------------------------------------------

names <- c("pdsi_mean", "ppt_mean", "ppt_pt", "pdsi_pt", "pdsi_dif")
cordat <- indat[,names]
chart.Correlation(cordat, histogram=T)

# Model misclassifications ------------------------------------------------

logr.class <- glm(mc ~ nhdclass, data=indat, family="binomial")
logr.dif <- glm(mc ~ abs(pdsi_dif), data=indat, family="binomial")
logr.pt <- glm(mc ~ pdsi_pt, data=indat, family="binomial")
logr.pdsi <- glm(mc ~ pdsi_mean, data=indat, family="binomial")
logr.pdsiclass <- glm(mc ~ pdsi_mean + nhdclass, data=indat, family="binomial")


# Predict with model ------------------------------------------------------

preds <- predict(logr.pdsi, newdata = data.frame(pdsi_mean=seq(-6,6,0.25)), type="response")
plot(seq(-6,6,0.25), preds, type="l", ylim=c(0,0.25))

predclass <- predict(logr.class, newdata = data.frame(nhdclass=c('Wet', 'Dry')), type="response")
