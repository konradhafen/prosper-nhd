
# Do setup ----------------------------------------------------------------

rm(list=ls())

setwd("E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\outputs\\csv")
fn <- "obs_hr_nhd_scpdsi.csv"

library(tidyverse)
#library(reshape2)
#library(plyr)

indat <- as.data.frame(read_csv(fn))
indat <- indat[indat$ppt_pt > 0,]

allobs <-as.data.frame(read_csv("all_obs_hr_nhd.csv"))

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

misclass_type <- function(nhdclass, obsclass)
{
  if (nhdclass == obsclass)
  {
    return ("Agree")
  }
  
  else
  {
    if (nhdclass == "Wet" & obsclass =="Dry")
    {
      return ("NHD wet Observation dry")
    }
    
    else if (nhdclass == "Dry" & obsclass =="Wet")
    {
      return ("NHD dry Observation wet")
    }
    
    else
    {
      return ("Invalid")
    }
    
  }
}

# Identify misclassifications ---------------------------------------------

indat$mc <- mapply(misclass, indat$FCODE, indat$Category)
indat$nhdclass <- mapply(nhdclass, indat$FCODE)
indat$wet <- ifelse(indat$nhdclass=="Wet", 1, 0)
indat$mctype <- mapply(misclass_type, indat$nhdclass, indat$Category)

allobs$mc <- mapply(misclass, allobs$FCODE, allobs$Category)
allobs$nhdclass <- mapply(nhdclass, allobs$FCODE)
allobs$mctype <- mapply(misclass_type, allobs$nhdclass, allobs$Category)
allobsmc <- subset(allobs, allobs$mc == 1)

perdat <- subset(indat, indat$nhdclass =='Wet')
intdat <- subset(indat, indat$nhdclass =='Dry')

agnhd <- indat %>% group_by(id) %>% summarise(fcode=mean(FCODE))
agnhd$nhdclass <- mapply(nhdclass, agnhd$fcode)
agdat <- indat %>% group_by(id) %>% summarise(wet=mean(wet))
agdat <- merge(agdat, agnhd[,c("id","nhdclass")], by.x="id", by.y="id")
agdat$mc <- ifelse((agdat$nhdclass=="Wet"&agdat$wet<1) | (agdat$nhdclass=="Dry"&agdat$wet==1), 1, 0)

# Correlation -------------------------------------------------------------

names <- c("pdsi_mean", "ppt_mean", "ppt_pt", "pdsi_pt", "pdsi_dif")
cordat <- indat[,names]
chart.Correlation(cordat, histogram=T)

# Model misclassifications ------------------------------------------------

moddat <- indat[indat$Month>7 & indat$Month<10 & indat$Year>0,]
logr.class <- glm(mc ~ nhdclass, data=indat, family="binomial")
logr.fcode <- glm(mc ~ as.factor(FCODE), data=indat, family="binomial")
logr.dif <- glm(mc ~ abs(pdsi_dif), data=indat, family="binomial")
logr.pt <- glm(mc ~ pdsi_pt, data=indat, family="binomial")
logr.pdsi <- glm(mc ~ pdsi_mean, data=indat, family="binomial")
logr.pdsiclass <- glm(mc ~ pdsi_mean + nhdclass, data=indat, family="binomial")
logr.pdsifcode <- glm(mc ~ pdsi_mean + as.factor(FCODE), data=indat, family="binomial")


# Predict with model ------------------------------------------------------

preds <- predict(logr.pdsi, newdata = data.frame(pdsi_mean=seq(-6,6,0.25)), type="response")
plot(seq(-6,6,0.25), preds, type="l", ylim=c(0,0.25), xlab="scPDSI during quad check year", ylab="Probability of misclassification")

predclass <- predict(logr.class, newdata = data.frame(nhdclass=c('Wet', 'Dry')), type="response")

fcode.df <- data.frame(FCODE=c(46003, 46006, 46007, 55800))
predfcode <- cbind(fcode.df, predict(logr.fcode, newdata=fcode.df, type="response"))


# Plot misclassifications by month ----------------------------------------

plotdat <- allobs
plotdat <- allobs[allobs$Month>0 & allobs$Month<13 & allobs$Year>0,]

#summary of plot data
plotsummary <- plotdat %>% group_by(mctype) %>% summarize(per=n()/nrow(plotdat))

#by type
ggplot(plotdat, aes(mctype)) +
  geom_bar(aes(y=(..count..)/sum(..count..), fill=mctype)) +
  scale_fill_manual(values=c("#03B935","#669BFF","#F9766E")) +
  scale_y_continuous(labels=scales::percent, breaks=seq(0,0.9,0.1)) +
  labs(x="", y="Percent of observations") +
  theme(legend.position = "none")

#by type, exclude "NHD dry Observation wet" before August
plotdat2 <- plotdat[!(plotdat$mctype=="NHD dry Observation wet" & plotdat$Month<8),]
plotsummary2 <- plotdat2 %>% group_by(mctype) %>% summarize(per=n()/nrow(plotdat2))
ggplot(plotdat2, aes(mctype)) +
  geom_bar(aes(y=(..count..)/sum(..count..), fill=mctype)) +
  scale_fill_manual(values=c("#03B935","#669BFF","#F9766E")) +
  scale_y_continuous(labels=scales::percent, breaks=seq(0,0.9,0.1)) +
  labs(x="", y="Percent of observations") +
  theme(legend.position = "none") +
  ggtitle("Excluding disagreement on NHD intermittent streams before August")

plotdat3 <- plotdat[!(plotdat$Category=="wet" & plotdat$Month<8),]
plotsummary3 <- plotdat3 %>% group_by(mctype) %>% summarize(per=n()/nrow(plotdat3))
ggplot(plotdat3, aes(mctype)) +
  geom_bar(aes(y=(..count..)/sum(..count..), fill=mctype)) +
  scale_fill_manual(values=c("#03B935","#669BFF","#F9766E")) +
  scale_y_continuous(labels=scales::percent, breaks=seq(0,0.9,0.1)) +
  labs(x="", y="Percent of observations") +
  theme(legend.position = "none") +
  ggtitle("Excluding all 'wet' observations before August")

#by year
ggplot(plotdat, aes((Year))) +
  geom_bar(aes(fill=mctype)) + 
  scale_fill_manual(values=c("#03B935","#669BFF","#F9766E")) +
  scale_x_continuous(breaks=seq(1976, 2016, 2)) +
  labs(x="Year", y="Observation count", fill="Misclassification") +
  theme(legend.position = c(1,1), legend.justification = c(1,1))

ggplot(plotdat[plotdat$Month>0,], aes(as.factor(Month))) + 
  geom_bar(aes(fill=mctype)) +
  scale_fill_manual(values=c("#03B935","#669BFF","#F9766E")) +
  labs(x="Month", y="Observation count", fill="Misclassification") +
  theme(legend.position = c(1,1), legend.justification = c(1,1))

ggplot(plotdat[plotdat$Month>0,], aes(as.factor(Month))) + 
  geom_bar(aes(fill=mctype)) +
  labs(x="Month", y="Observations") +
  facet_wrap(~FCODE, ncol=2) 
  #geom_bar(aes(y=(..count..)/sum(..count..), fill=mctype)) + 
  #scale_y_continuous(labels=scales::percent)

ggplot(plotdat[plotdat$Month>0,], aes(as.factor(Month))) + 
  geom_bar(aes(y=(..count..)/sum(..count..), fill=mctype)) + 
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~FCODE, ncol=2)

ggplot(plotdat[plotdat$Year>0,], aes(Year)) + 
  geom_bar(aes(y=(..count..)/sum(..count..), fill=mctype)) + 
  scale_y_continuous(labels=scales::percent)


# Aggregate by month ------------------------------------------------------

tempdat <- allobs[allobs$Year>0 & allobs$Month>0,]

monthdat <- tempdat %>% group_by(Month) %>% summarise (ct=n(), ndow=sum(mctype=="NHD dry Observation wet"),
                                                       nwod=sum(mctype=="NHD wet Observation dry"))
monthdat$pndow <- monthdat$ndow/monthdat$ct
monthdat$pnwod <- monthdat$nwod/monthdat$ct
monthdat.sub <- monthdat[,c("Month","pndow","pnwod")]
n <- melt(cbind(monthdat$ndow, monthdat$nwod))
colnames(n) <- c("var1", "var2", "count")
monthdat.melt <- cbind(melt(monthdat.sub, id=c("Month")), n$count)
monthdat.melt$variable <- factor(monthdat.melt$variable, levels=c("pndow", "pnwod"), 
                                 labels=c("NHD intermittent Observation wet", "NHD perennial Observation dry"))

monthdat.melt$pos <- c(rep(0.075,12), rep(0.0075,12))

ggplot(monthdat.melt, aes(as.factor(Month))) +
  geom_bar(aes(x=Month, y=value, fill=variable), stat="identity") + 
  scale_fill_manual(values=c("#669BFF","#F9766E")) +
  geom_text(aes(x=Month, y=pos, label=n$count)) +
  scale_x_continuous(breaks=seq(1,12,1)) +
  scale_y_continuous(labels=scales::percent) +
  labs(x="Month", y="Percent of total field observations") +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()) +
  ggtitle("Difference between NHD Classifications and Field Observations")

