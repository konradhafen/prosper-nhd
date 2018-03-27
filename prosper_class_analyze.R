
# Do setup ----------------------------------------------------------------

rm(list=ls())
library(ggplot2)
library(reshape2)

wd <- "E:/konrad/Projects/usgs/prosper-nhd/data/outputs/csv"
fn <- "nhd_mr_class_buf20.csv"

indat <- read.csv(paste(wd,fn,sep="/"))


# Subset to perennial/intermittnet only and classify overall --------------

pidat <- subset(indat, indat$FCODE==46006 | indat$FCODE==46003)
pidat$nclass <- ifelse(pidat$FCODE==46006, "Perennial", "Intermittent")

#classify reaches as perennial or intermittent based on pwet

#cutoff proportion for perennial
cutoff <- 0.75
pidat$pclass <- ifelse(pidat$pwet >= cutoff, "Wet", "Dry")
#run functions section first
pidat$dclass <- mapply(misclass_type, pidat$nclass, pidat$pclass)

#melt
pidat.melt <- melt(pidat)


# functions ---------------------------------------------------------------

misclass_type <- function(nhdclass, prosperclass)
{
  if ((nhdclass == "Perennial" & prosperclass=="Wet") | (nhdclass == "Intermittent" & prosperclass=="Dry"))
  {
    return ("same")
  }
  else if ((nhdclass == "Perennial" & prosperclass=="Dry"))
  {
    return ("nhd wet prosper dry")
  }
  else if ((nhdclass == "Intermittent" & prosperclass=="Wet"))
  {
    return("nhd dry prosper wet")
  }
}

