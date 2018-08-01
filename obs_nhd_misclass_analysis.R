
# Do setup ----------------------------------------------------------------

rm(list=ls())
options(scipen=999)

setwd("E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\outputs\\csv")

#contains info from points where quad data a climate data exist
fn <- "obs_hr_nhd_scpdsi.csv"

library(tidyverse)
library(broom)
#library(reshape2)
#library(plyr)

indat <- as.data.frame(read_csv(fn))
indat <- indat[indat$ppt_pt > 0,]

allobs <-as.data.frame(read_csv("all_obs_hr_nhd.csv"))
allmrhr <- as.data.frame(read_csv("all_obs_mr_hr.csv"))

# remove observation where FCODE may be incorrect
indat <- indat[indat$FID != 11120,]
indat$ppt_dif <- indat$ppt_mean - indat$ppt_pt
allobs <- allobs[allobs$FID != 8501,]
indat.dry <- indat[!(indat$Category=="Wet" & indat$Month<8),]

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
    return('Permanent')
  }
  else
  {
    return('Nonpermanent')
  }
}

misclass_type <- function(nhdclass, obsclass)
{
  if ((nhdclass == "Permanent" & obsclass == "Wet") | (nhdclass == "Nonpermanent" & obsclass == "Dry"))
  {
    return ("Agree")
  }
  
  else
  {
    if (nhdclass == "Permanent" & obsclass =="Dry")
    {
      return ("NHD wet Observation dry")
    }
    
    else if (nhdclass == "Nonpermanent" & obsclass =="Wet")
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
indat$wet <- ifelse(indat$nhdclass=="Permanent", 1, 0)
indat$mctype <- mapply(misclass_type, indat$nhdclass, indat$Category)

allobs$mc <- mapply(misclass, allobs$FCODE, allobs$Category)
allobs$nhdclass <- mapply(nhdclass, allobs$FCODE)
allobs$mctype <- mapply(misclass_type, allobs$nhdclass, allobs$Category)
allobsmc <- subset(allobs, allobs$mc == 1)

allmrhr$mcmr <- mapply(misclass, allmrhr$FCODE, allmrhr$Category)
allmrhr$nhdclassmr <- mapply(nhdclass, allmrhr$FCODE)
allmrhr$mctypemr <- mapply(misclass_type, allmrhr$nhdclassmr, allmrhr$Category)
allmrhr$mchr <- mapply(misclass, allmrhr$FCODE_1, allmrhr$Category)
allmrhr$nhdclasshr <- mapply(nhdclass, allmrhr$FCODE_1)
allmrhr$mctypehr <- mapply(misclass_type, allmrhr$nhdclasshr, allmrhr$Category)

perdat <- subset(indat, indat$nhdclass =='Permanent')
intdat <- subset(indat, indat$nhdclass =='Nonpermanent')

agnhd <- indat %>% group_by(id) %>% summarise(fcode=mean(FCODE))
agnhd$nhdclass <- mapply(nhdclass, agnhd$fcode)
agdat <- indat %>% group_by(id) %>% summarise(wet=mean(wet))
agdat <- merge(agdat, agnhd[,c("id","nhdclass")], by.x="id", by.y="id")
agdat$mc <- ifelse((agdat$nhdclass=="Permanent"&agdat$wet<1) | (agdat$nhdclass=="Nonpermanent"&agdat$wet==1), 1, 0)

agct <- indat %>% group_by(id) %>% summarise(n=n(), tot=sum(wet), per=tot/n)


# Save csv ----------------------------------------------------------------

writeobs <- allobs[,c("FID", "nhdclass", "mc", "mctype")]
write_csv(writeobs, "misclassifications_obs_nhd.csv")

# Correlation -------------------------------------------------------------

names <- c("pdsi_mean", "ppt_mean", "ppt_pt", "pdsi_pt", "pdsi_dif")
cordat <- indat[,names]
chart.Correlation(cordat, histogram=T)

# Model misclassifications ------------------------------------------------

moddat <- indat[indat$Month>7 & indat$Month<10 & indat$Year>0,]

#exclude wet observations before August
moddat <- indat[!(indat$Category=="Wet" & indat$Month<8),]

#REMEMBER: coefficients are log odds
indat.dry$apdsi_dif <- abs(indat.dry$pdsi_dif)
indat.dry$appt_dif <- abs(indat.dry$ppt_dif)
lr.pdsi <- glm(mc ~ Category + pdsi_mean, data=indat.dry, family=binomial)
lr.pdsidif <- glm(mc ~ Category + apdsi_dif, data=indat.dry, family=binomial)
lr.pdsidifint<- glm(mc ~ Category * apdsi_dif, data=indat.dry, family=binomial)
lr.ppt <- glm(mc ~ Category + ppt_pt, data=indat.dry, family=binomial)
lr.pptint <- glm(mc ~ Category * ppt_pt, data=indat.dry, family=binomial)
lr.pptdif <- glm(mc ~ Category + appt_dif, data=indat.dry, family=binomial)

#cooks distance
plot(lr.pdsidifint, which=4, id.n=3)

#extract model results for additive model
model.data <- augment(lr.pdsidif) %>% mutate(index = 1:n())
score <- qnorm((0.95/2) + 0.5)
model.data$lwr <- plogis(model.data$.fitted-score*model.data$.se.fit)
model.data$upr <- plogis(model.data$.fitted+score*model.data$.se.fit)
ggplot(model.data, aes(apdsi_dif, plogis(.fitted))) +
  geom_ribbon(aes(x=apdsi_dif, ymin=lwr, ymax=upr, group=Category), alpha = 0.2) + 
  geom_line(aes(color=Category)) + 
  theme_bw()
  

#extract model results for interactive model
model.data <- augment(lr.pdsidifint) %>% mutate(index = 1:n())

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = as.factor(mc)), alpha = .5) +
  theme_bw()

score <- qnorm((0.95/2) + 0.5)
model.data$lwr <- plogis(model.data$.fitted-score*model.data$.se.fit)
model.data$upr <- plogis(model.data$.fitted+score*model.data$.se.fit)
dry <- model.data[model.data$Category=="Dry",]
ggplot(model.data, aes(apdsi_dif, plogis(.fitted))) +
  geom_ribbon(aes(x=apdsi_dif, ymin=lwr, ymax=upr, group=Category), alpha=0.25) +
  geom_line(aes(color = Category), size=1) +
  scale_x_continuous(breaks = seq(0,9,1)) +
  scale_y_continuous(breaks = seq(0,0.7,0.1)) +
  labs(x="scPDSI difference (absolute value)", y="Probability of NHD disagreement") +
  theme_bw()

# Predict with model ------------------------------------------------------

dat.pred <- rbind(data.frame(Category="Dry", apdsi_dif=seq(0,9,0.1)), data.frame(Category="Wet", apdsi_dif=seq(0,9,0.1)))
preds <- predict(lr.pdsidifint, newdata=dat.pred, type="response")
dat.pred$pred <- preds
plot(dat.pred[dat.pred$Category=="Dry",]$apdsi_dif, dat.pred[dat.pred$Category=="Dry",]$pred, type="l", ylim=c(0,0.75), col="red")
lines(dat.pred[dat.pred$Category=="Wet",]$apdsi_dif, dat.pred[dat.pred$Category=="Wet",]$pred, col="blue")

preds <- predict(lr.pdsi, newdata = data.frame(pdsi_mean=seq(-6,6,0.25)), type="response")
plot(seq(-6,6,0.25), preds, type="l", ylim=c(0,0.25), xlab="scPDSI during quad check year", ylab="Probability of misclassification")

predclass <- predict(lr.class, newdata = data.frame(nhdclass=c('Permanent', 'Nonpermanent')), type="response")

fcode.df <- data.frame(FCODE=c(46003, 46006, 46007, 55800))
predfcode <- cbind(fcode.df, predict(lr.fcode, newdata=fcode.df, type="response"))



# Confusion matrices (NHD vs Observed) ------------------------------------

#plotdat <- allobs[!(allobs$Category=="Wet" & allobs$Month == 0),]
plotdat.year <- allobs[allobs$Year > 0,]
plotdat.date <- allobs[allobs$Month>0 & allobs$Month<13 & allobs$Year>0,]
plotdat.dry <- allobs[!(allobs$Category=="Wet" & allobs$Month<8),]
indat.dry <- indat[!(indat$Category=="Wet" & indat$Month<8),]

#summary tables
table.all <- table(allobs$nhdclass, allobs$Category)
prop.table(table.all) * 100

table.year <- table(plotdat.year$nhdclass, plotdat.year$Category)
prop.table(table.year) * 100

table.date <- table(plotdat.date$nhdclass, plotdat.date$Category)
prop.table(table.date) * 100

table.dry <- table(plotdat.dry$nhdclass, plotdat.dry$Category)
prop.table(table.dry) * 100


# Cross validated comparison of NHD and Observations ----------------------

minobs <- 3259 #minimum of wet and dry observations
nsample <- 2000 # number of samples to take from wet and dry points
wetobs <- plotdat.dry[plotdat.dry$Category=="Wet",]
dryobs <- plotdat.dry[plotdat.dry$Category=="Dry",]
results <- data.frame(Agree=NA, NHDdryObsWet=NA, NHDwetObsDry=NA)

for (i in 1:10)
{
  obssample <- rbind(wetobs[sample(nrow(wetobs), nsample),], dryobs[sample(nrow(dryobs), nsample),])
  results[i,] <- list(sum(obssample$mctype=="Agree"), sum(obssample$mctype=="NHD dry Observation wet"),
                              sum(obssample$mctype=="NHD wet Observation dry"))
}

results.summary <- as.data.frame(colMeans(results)/(nsample*2)*100)
names(results.summary) <- c("value")
results.summary$mctype <- unique(allobs$mctype)
results.summary

ggplot(results.summary, aes(x=mctype, y=value)) +
  geom_bar(stat="identity") + 
  labs(x="", y="Percent") +
  ggtitle("Mean of 10 random samples (n=2000) of wet (after July) and dry points")


# Cross validated comparison of MR/HR and Observations --------------------

nsample <- 2000 # number of samples to take from wet and dry points
mrhr.dry <- allmrhr[!(allmrhr$Category=="Wet" & allmrhr$Month<8),]
wetobs <- mrhr.dry[mrhr.dry$Category=="Wet",]
dryobs <- mrhr.dry[mrhr.dry$Category=="Dry",]
resultshr <- data.frame(Agree=NA, NHDdryObsWet=NA, NHDwetObsDry=NA)
resultsmr <- data.frame(Agree=NA, NHDdryObsWet=NA, NHDwetObsDry=NA)

for (i in 1:10)
{
  obssample <- rbind(wetobs[sample(nrow(wetobs), nsample),], dryobs[sample(nrow(dryobs), nsample),])
  resultsmr[i,] <- list(sum(obssample$mctypemr=="Agree"), sum(obssample$mctypemr=="NHD dry Observation wet"),
                      sum(obssample$mctypemr=="NHD wet Observation dry"))
  resultshr[i,] <- list(sum(obssample$mctypehr=="Agree"), sum(obssample$mctypehr=="NHD dry Observation wet"),
                        sum(obssample$mctypehr=="NHD wet Observation dry"))
}

resultsmr.summary <- as.data.frame(colMeans(resultsmr)/(nsample*2)*100)
names(resultsmr.summary) <- c("value")
resultsmr.summary$mctype <- c("Agree", "NHD dry Observation Wet", "NHD wet Observation dry")
resultsmr.summary$version <- "MR-NHD"
resultsmr.summary

resultshr.summary <- as.data.frame(colMeans(resultshr)/(nsample*2)*100)
names(resultshr.summary) <- c("value")
resultshr.summary$mctype <- c("Agree", "NHD dry Observation Wet", "NHD wet Observation dry")
resultshr.summary$version <- "HR-NHD"
resultshr.summary

results <- rbind(resultshr.summary, resultsmr.summary)

ggplot(results, aes(x=mctype, y=value)) +
  geom_bar(stat="identity") + 
  facet_wrap(~version, ncol=1) +
  labs(x="", y="Percent") +
  ggtitle("Mean of 10 random samples (n=2000) of wet (after July) and dry points")

# Logistic regression with a balanced dataset -----------------------------

indat.dry <- indat[!(indat$Category=="Wet" & indat$Month<8),]
wetobs <- indat.dry[indat.dry$Category=="Wet",]
dryobs <- indat.dry[indat.dry$Category=="Dry",]
nsample <- 2800

for (i in 1:10)
{
  obssample <- rbind(wetobs[sample(nrow(wetobs), nsample),], dryobs[sample(nrow(dryobs), nsample),])
  lr.pdsi.balanced <- glm(mc ~ pdsi_mean, data=obssample, family="binomial")
  print(paste(i, summary(lr.pdsi.balanced)$coefficients[2,1], summary(lr.pdsi.balanced)$coefficients[2,4]))
}


# Plot misclassifications spatially ---------------------------------------

#HUC6
plotdat.dry$HUC_6 <- as.numeric(substr(plotdat.dry$HUC_8, 1, 6))
ggplot(plotdat.dry, aes(as.factor(HUC_6))) +
  scale_fill_manual(values=c("#52de01","#04a9fc","#ff1904"), name="Observation type") +
  geom_bar(aes(fill=mctype)) +
  labs(x="HUC 6", y="Observations") + 
  theme(axis.text.x = element_text(angle=-90, vjust=0.5), legend.position = c(0.13,0.84))

#HUC8
ggplot(plotdat.dry, aes(as.factor(HUC_8))) +
  geom_bar(aes(fill=mctype)) +
  labs(x="", y="Observations")

#HUC12
ggplot(plotdat.dry, aes(as.factor(HUC_12))) +
  geom_bar(aes(fill=mctype)) +
  scale_y_continuous(breaks=seq(0,50,10)) +
  labs(x="")


# Aggregate by HUC --------------------------------------------------------

aghuc12 <- plotdat.dry %>% group_by(HUC_12) %>% summarise(misclass=mean(mc))
write_csv(aghuc12, "misclassification_huc12.csv")

aghuc8 <- plotdat.dry %>% group_by(HUC_8) %>% summarise(misclass=mean(mc))
write_csv(aghuc8, "misclassification_huc8.csv")


# Aggregate by quad -------------------------------------------------------

agquad <- indat.dry %>% group_by(US_7_ID) %>% summarise(misclass=mean(mc), count=n())
write_csv(agquad, "misclassification_quad.csv")
agquad.plot <- agquad[agquad$count > 0,]
agquad.plot1 <- agquad[agquad$count == 1,]
agquad.plot1g <- agquad[agquad$count > 10,]

# library(reshape2)
# agquad.plot.melt <- melt(agquad.plot[,2:3])

ggplot(agquad.plot, aes(x=count)) +
  geom_histogram(binwidth = 1) + 
  labs(x="Observations per quad", y="Count")

ggplot(agquad.plot1g, aes(x=misclass)) +
  geom_histogram(binwidth = 0.025) +
  labs(x="Percentage of misclassifications", y="Count")


# Plot misclassifications by month ----------------------------------------

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
plotdat2 <- allobs[!(allobs$mctype=="NHD dry Observation wet" & allobs$Month<8),]
plotsummary2 <- plotdat2 %>% group_by(mctype) %>% summarize(per=n()/nrow(plotdat2))
ggplot(plotdat2, aes(mctype)) +
  geom_bar(aes(y=(..count..)/sum(..count..), fill=mctype)) +
  scale_fill_manual(values=c("#03B935","#669BFF","#F9766E")) +
  scale_y_continuous(labels=scales::percent, breaks=seq(0,0.9,0.1)) +
  labs(x="", y="Percent of observations") +
  theme(legend.position = "none") +
  ggtitle("Excluding disagreement on NHD intermittent streams before August")

plotsummary3 <- plotdat3 %>% group_by(mctype) %>% summarize(per=n()/nrow(plotdat3))
ggplot(plotdat.dry, aes(mctype)) +
  geom_bar(aes(y=(..count..)/sum(..count..), fill=mctype)) +
  scale_fill_manual(values=c("#03B935","#669BFF","#F9766E")) +
  scale_y_continuous(labels=scales::percent, breaks=seq(0,0.9,0.1)) +
  labs(x="", y="Percent of observations") +
  theme(legend.position = "none") +
  ggtitle("Excluding all 'wet' observations before August")

#by year
ggplot(plotdat.year, aes((Year))) +
  geom_bar(aes(fill=mctype)) + 
  scale_fill_manual(values=c("#03B935","#669BFF","#F9766E")) +
  scale_x_continuous(breaks=seq(1976, 2016, 2)) +
  labs(x="Year", y="Observation count", fill="Misclassification") +
  theme(legend.position = c(1,1), legend.justification = c(1,1))

#by month
ggplot(plotdat.date, aes(as.factor(Month))) + 
  geom_bar(aes(fill=mctype)) +
  scale_fill_manual(values=c("#03B935","#669BFF","#F9766E")) +
  labs(x="Month", y="Observation count", fill="Misclassification") +
  theme(legend.position = c(1,1), legend.justification = c(1,1))

#by month and fcode
ggplot(plotdat.date, aes(as.factor(Month))) + 
  geom_bar(aes(fill=mctype)) +
  labs(x="Month", y="Observations") +
  facet_wrap(~FCODE, ncol=2) 
  #geom_bar(aes(y=(..count..)/sum(..count..), fill=mctype)) + 
  #scale_y_continuous(labels=scales::percent)

#by month and fcode, percentages
ggplot(plotdat[plotdat$Month>0,], aes(as.factor(Month))) + 
  geom_bar(aes(y=(..count..)/sum(..count..), fill=mctype)) + 
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~FCODE, ncol=2)

#by year percentages
ggplot(plotdat.year, aes(Year)) + 
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


# Scatter plot of misclassification type and climate conditions -----------

ggplot(indat.dry, aes(x=pdsi_dif, y=ppt_dif)) + 
  geom_point(aes(color=as.factor(mctype))) + 
  labs(x="PDSI difference", y="Precipitation percentile difference", color="Disagreement") +
  ggtitle("Difference")

ggplot(indat.dry, aes(x=pdsi_mean, y=ppt_mean)) + 
  geom_point(aes(color=as.factor(mctype))) + 
  labs(x="PDSI", y="Precipitation percentile", color="Disagreement") +
  ggtitle("Climate during NHD collection")

ggplot(indat.dry, aes(x=pdsi_pt, y=ppt_pt)) + 
  geom_point(aes(color=as.factor(mctype))) +
  labs(x="PDSI", y="Precipitation percentile", color="Disagreement") +
  ggtitle("Climate during field obs collection")


# Box plots of misclassification type and climate conditions --------------

ggplot(indat.dry, aes(x=as.factor(mctype), y=ppt_dif)) + 
  geom_boxplot() + 
  labs(x="", y="Difference in precip percentile") + 
  ggtitle("Difference between NHD collection year and field observation year")

ggplot(indat.dry, aes(x=as.factor(mctype), y=ppt_pt)) + 
  geom_boxplot() + 
  labs(x="", y="Precipitation percentile") + 
  ggtitle("NHD collection year")

ggplot(indat.dry, aes(x=as.factor(mctype), y=ppt_mean)) + 
  geom_boxplot() + 
  labs(x="", y="Precipitation percentile") + 
  ggtitle("Field observation year")

ggplot(indat.dry, aes(x=as.factor(mctype), y=pdsi_dif)) + 
  geom_boxplot() + 
  labs(x="", y="Difference in PDSI") + 
  ggtitle("Difference between NHD collection year and field observation year")

ggplot(indat.dry, aes(x=as.factor(mctype), y=pdsi_pt)) + 
  geom_boxplot() + 
  labs(x="", y="PDSI") + 
  ggtitle("Field observation year")

ggplot(indat.dry, aes(x=as.factor(mctype), y=pdsi_mean)) + 
  geom_boxplot() + 
  labs(x="", y="PDSI") + 
  ggtitle("NHD collection year")
