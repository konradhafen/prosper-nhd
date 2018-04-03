# Do setup ----------------------------------------------------------------

rm(list=ls())
library(ggplot2)
library(reshape2)

wd <- "E:/konrad/Projects/usgs/prosper-nhd/data/outputs/csv"
fn <- "nhd_prosper_cat_quads._mr.csv"

indat <- read.csv(paste(wd,fn,sep="/"))


# Misclassification regression --------------------------------------------


