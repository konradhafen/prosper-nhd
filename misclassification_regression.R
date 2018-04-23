# Do setup ----------------------------------------------------------------

rm(list=ls())
library(ggplot2)
library(reshape2)

wd <- "E:/konrad/Projects/usgs/prosper-nhd/data/outputs/csv"
fn <- "nhd_prosper_cat_quads_mr.csv"

indat <- read.csv(paste(wd,fn,sep="/"))


# Misclassification regression --------------------------------------------

lm1 <- lm(disagree ~ pdsi_mean, data=indat)
summary(lm1)

plot(indat$pdsi_mean, indat$disagree)
abline(0.329, 0.0018)
