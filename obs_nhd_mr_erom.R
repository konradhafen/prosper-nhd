
# Do setup ----------------------------------------------------------------

rm(list=ls())
options(scipen=999)

setwd("E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\outputs\\csv")

#contains info from points where quad data a climate data exist
fn <- "obs_mr_nhd_erom.csv"
fn2 <- "obs_mr_nhd_erom_monthly.csv"

library(tidyverse)
library(reshape2)

indat <- as.data.frame(read_csv(fn))
indat <- indat[!is.na(indat$Q0001F),]
indat.month <- as.data.frame(read_csv(fn2))
indat.month <- indat.month[!is.na(indat.month$Q0001F),]
indat.month <- indat.month[!(indat.month$Category=="Wet" & indat.month$Month<8),]
keeps.month <- c("Category", "FCODE", "Q0001F_1", "Q0001F_12", "Q0001F__13", "Q0001F__14", "Q0001F__15", "Q0001F__16",
                 "Q0001F__17", "Q0001F__18", "Q0001F__19", "Q0001F__20", "Q0001F__21", "Q0001F__22")
indat.month <- indat.month[,keeps.month]
colnames(indat.month) <- c("Category", "FCODE", "1", "2", "3", "4", "5", "6",
                           "7", "8", "9", "10", "11", "12")

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
  facet_wrap(~Category, ncol=2) + 
  labs(x="", y="Mean annual flow log(Q0001F)")

ggplot(indat.lowq, aes(x=nhdclass, y=Q0001F)) + 
  geom_boxplot() + 
  facet_wrap(~Category, ncol=2)

ggplot(indat.lowq, aes(x=Category, y=Q0001F)) + 
  geom_boxplot() +
  facet_wrap(~nhdclass, ncol=2)


# Monthly boxplots --------------------------------------------------------

indat.month$nhdclass <- mapply(nhdclass, indat.month$FCODE)

indat.melt <- melt(indat.month, id = c("Category", "nhdclass"), measure.vars = c("1", "2", "3", "4", "5", "6",
                                       "7", "8", "9", "10", "11", "12"))
indat.melt$groups <- interaction(indat.melt$Category, indat.melt$nhdclass)

ggplot(data=indat.melt, aes(y=log(value), x=variable, fill=Category)) + 
  geom_boxplot() + 
  facet_wrap(~nhdclass, ncol = 1)
