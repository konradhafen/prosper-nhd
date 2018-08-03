
# Do setup ----------------------------------------------------------------

rm(list=ls())
options(scipen=999)

setwd("E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\outputs\\csv")

#contains info from points where quad data a climate data exist
fn <- "obs_mr_nhd_erom.csv"

library(tidyverse)

indat <- as.data.frame(read_csv(fn))
indat <- indat[!is.na(indat$Q0001F),]

indat.dry <- indat[!(indat$Category=="Wet" & indat$Month<8),]
indat.lowq <- indat[!(indat$Category=="Wet" & indat$Month<8) & indat$Q0001F < 250,]


# Classify reaches as permaenet or nonpermanent ---------------------------

indat$nhdclass <- mapply(nhdclass, indat$FCODE)


# Boxplots of flow by NHD class -------------------------------------------

ggplot(indat, aes(x=nhdclass, y=Q0001F)) + 
  geom_boxplot()

ggplot(indat, aes(log(Q0001F))) +
  geom_histogram() +
  facet_wrap(~nhdclass)

ggplot(indat.dry, aes(x=nhdclass, y=log(Q0001F))) + 
  geom_boxplot() + 
  facet_wrap(~Category, ncol=2)

ggplot(indat.lowq, aes(x=nhdclass, y=Q0001F)) + 
  geom_boxplot() + 
  facet_wrap(~Category, ncol=2)

ggplot(indat.lowq, aes(x=Category, y=Q0001F)) + 
  geom_boxplot() +
  facet_wrap(~nhdclass, ncol=2)
