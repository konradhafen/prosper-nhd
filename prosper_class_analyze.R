
# Do setup ----------------------------------------------------------------

rm(list=ls())
library(ggplot2)
library(reshape2)

wd <- "E:/konrad/Projects/usgs/prosper-nhd/data/outputs/csv"
fn <- "nhd_mr_class_buf20.csv"

indat <- read.csv(paste(wd,fn,sep="/"))


# Subset to perennial/intermittnet only and classify overall --------------

pidat <- subset(indat, indat$FCODE==46006 | indat$FCODE==46003)

#classify reaches as perennial or intermittent based on pwet

#cutoff proportion for perennial
cutoff <- 0.75
pidat$class <- ifelse(pidat$pwet >= cutoff, "Perennial", "Intermittent")

#melt
pidat.melt <- melt(pidat)


# Analysis ----------------------------------------------------------------


