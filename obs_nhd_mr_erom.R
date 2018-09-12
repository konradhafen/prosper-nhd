
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
colnames(indat.month) <- c("Category", "FCODE", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

indat.dry <- indat[!(indat$Category=="Wet" & indat$Month<8),]
indat.lowq <- indat[!(indat$Category=="Wet" & indat$Month<8) & indat$Q0001F < 250,]


# Classify reaches as permaenet or nonpermanent ---------------------------

indat$nhdclass <- mapply(nhdclass, indat$FCODE)
indat.month$nhdclass <- mapply(nhdclass, indat.month$FCODE)
indat.month$mctype <- mapply(misclass_type, indat.month$nhdclass, indat.month$Category)
indat.month$mc <- ifelse(indat.month$mctype == "Agree", 0, 1)


# Test misclass as a function of baseflow (sept flow) ---------------------

library(broom)
library(pscl)

indat.month$bfzero <- ifelse(indat.month$Sep == 0, 1, 0)

lr.cat <- glm(mc ~ Category, data=indat.month, family="binomial")
lr.bf <- glm(mc ~ Sep, data=indat.month, family="binomial")
lr.catbf <- glm(mc ~ Category + Sep, data=indat.month, family="binomial")
lr.catbfint <- glm(mc ~ Category*Sep, data=indat.month, family="binomial")
lr.0 <- glm(mc ~ bfzero, data=indat.month, family="binomial")
lr.cat0  <- glm(mc ~ Category + bfzero, data=indat.month, family="binomial")
lr.cat0int <- glm(mc ~ Category*bfzero, data=indat.month, family="binomial")

pR2(lr.cat0int)
pR2(lr.cat)
pR2(lr.0)

AICctab(lr.cat, lr.0, lr.cat0, lr.cat0int)

plot.dat <- augment(lr.cat0int)
ggplot(plot.dat, aes(bfzero, plogis(.fitted))) +
  geom_line(aes(color=Category)) + 
  geom_point(aes(bfzero, mc, colour=Category), alpha=0.1) +
  labs(x = "EROM Sep", y = "Probability of disagreement") +
  theme_bw()


# Create ROC curve --------------------------------------------------------

library(pROC)
roc.data <- augment(lr.cat0int)
roc.fit <- roc(mc ~ plogis(.fitted), data=roc.data)
auc(roc.fit)
plot.roc(roc.fit, xlim=c(0,1), ylim=c(0,1))


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
indat.melt$logvalue <- log(indat.melt$value + 0.01)

ggplot(data=indat.melt, aes(y=logvalue, x=variable, fill=Category)) + 
  geom_boxplot() + 
  facet_wrap(~nhdclass, ncol = 1) + 
  labs(y = "ln[Q] cfs", x = "Month")

ggplot(data=indat.melt, aes(y=logvalue, x=variable, fill=groups)) + 
  geom_boxplot() + 
  labs(y = "ln[Q] cfs", x = "Month")
