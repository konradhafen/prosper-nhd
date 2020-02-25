
# Load data ---------------------------------------------------------------

rm(list=ls())
options(scipen=999)

setwd("E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\outputs\\csv")

#contains info from points where quad data a climate data exist
fn <- "obs_hr_nhd_scpdsi_p.csv"

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


# Plot by month -----------------------------------------------------------

#plotdat.datedry <- allobs[!(allobs$Category=="Wet" & allobs$Month<8)&allobs$Month>0,]
plotdat <- allobs
levels(plotdat$Category) <- c("Dry", "Wet")
plotdat$Category <- factor(plotdat$Category, levels=rev(levels(plotdat$Category)))


ggplot(plotdat, aes(as.factor(Month))) + 
  geom_bar(aes(fill=Category)) +
  scale_fill_manual(values=c("#0c51fd","#fb0026")) +
  scale_x_discrete(breaks=0:12, labels=c("Not\nrecorded", "Jan", "Feb", "Mar", "Apr", 
                                           "May", "Jun", "Jul", "Aug", "Sep", "Oct", 
                                           "Nov", "Dec")) +
  labs(x="Month", y="Observation count") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"), axis.line = element_line(colour = "black"),
        axis.title=element_text(size=14), axis.text=element_text(size=12),
        plot.title=element_text(size=16, face="bold", margin=margin(t=0, r=0, b=20, l=0)),
        plot.background = element_rect(fill = "transparent", color = NA), 
        axis.title.y = element_text(margin=margin(t=0, r=20, b=0, l=0)),
        axis.title.x = element_text(margin=margin(t=10, r=0, b=0, l=0))) +
  theme(legend.position = c(0.98,0.98), legend.justification = c(1,1), legend.title = element_blank())

#Plot dimensions 700x375 

# Save plot ---------------------------------------------------------------

ggsave("C:/Users/khafe/Downloads/disagreement_month_hr.png", plot = last_plot(), width = 7, height = 4.5, units = "in", bg = "transparent")


# Plot disagreement for MR and HR -----------------------------------------

plotdat <- allmrhr[!(allmrhr$Category=="Wet" & (allmrhr$Month<8 | allmrhr$Month>9)),]

plotdat$labhr <- plotdat$mctypehr
plotdat$labhr <- ifelse(plotdat$labhr == "NHD dry Observation wet", 2, plotdat$labhr)
plotdat$labhr <- ifelse(plotdat$labhr == "NHD wet Observation dry", 3, plotdat$labhr)
plotdat$labhr <- ifelse(plotdat$labhr > 1 & plotdat$labhr < 4,  plotdat$labhr, 1)

plotdat$labmr <- plotdat$mctypemr
plotdat$labmr <- ifelse(plotdat$labmr == "NHD dry Observation wet", 2, plotdat$labmr)
plotdat$labmr <- ifelse(plotdat$labmr == "NHD wet Observation dry", 3, plotdat$labmr)
plotdat$labmr <- ifelse(plotdat$labmr > 1 & plotdat$labmr < 4,  plotdat$labmr, 1)

#melt the data
library(reshape2)
meltdat = plotdat[,c("labmr", "labhr")]
colnames(meltdat) <- c('NHD-MR', 'NHD-HR')
meltdat <- melt(meltdat, measure.vars = c('NHD-MR', 'NHD-HR'))

ggplot(meltdat, aes(x=value, y=2*100*(..count..)/sum(..count..), fill=variable)) +
  geom_bar(position='dodge', colour='black') +
  labs(x="", y="Percent of observations") +
  scale_x_discrete(breaks=1:3, labels=c("Agree", "NHD non-perennial,\nObservation wet", "NHD perennial,\nObservation dry")) +
  scale_fill_manual(values=c('gray45', 'white')) +
  theme(legend.position = c(0.98,0.98), legend.justification = c(1,1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"), axis.line = element_line(colour = "black"),
        plot.title=element_text(size=16, face="bold", margin=margin(t=0, r=0, b=20, l=0)),
        axis.title=element_text(size=14), axis.text=element_text(size=12), 
        plot.background = element_rect(fill = "transparent", color = NA), 
        axis.title.y = element_text(margin=margin(t=0, r=20, b=0, l=0)),
        legend.position = c(0.98,0.98), legend.justification = c(1,1),
        legend.title = element_blank(), 
        legend.text = element_text(size=12))
ggsave("C:/Users/khafe/Downloads/disagreement_all_v2.png", plot = last_plot(), 
       width = 8, height = 6, units = "in", bg = "transparent")
ggsave("E:/konrad/Projects/usgs/prosper-nhd/figs/figs/disagreement_all_v2.png", plot = last_plot(), 
       width = 8, height = 6, units = "in", bg = "white")


# Plot total disagreement -------------------------------------------------

plotdat <- allmrhr[!(allmrhr$Category=="Wet" & allmrhr$Month<8),]

plotdat$lab <- plotdat$mctypehr
plotdat$lab <- ifelse(plotdat$lab == "NHD dry Observation wet", "NHD non-perennial,\nObservation wet", plotdat$lab)
plotdat$lab <- ifelse(plotdat$lab == "NHD wet Observation dry", "NHD perennial,\nObservation dry", plotdat$lab)

ggplot() +
  geom_bar(data=plotdat, aes(x=lab, y=100*(..count..)/sum(..count..)), width=0.65) +
  labs(x="", y="Percent of observations") +
  ggtitle("Disagreement with NHDPlus-HR") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"), axis.line = element_line(colour = "black"),
        plot.title=element_text(size=16, face="bold", margin=margin(t=0, r=0, b=20, l=0)),
        axis.title=element_text(size=14), axis.text=element_text(size=12), 
        plot.background = element_rect(fill = "transparent", color = NA), 
        axis.title.y = element_text(margin=margin(t=0, r=20, b=0, l=0)))

ggsave("C:/Users/khafe/Downloads/disagreement_hr.png", plot = last_plot(), width = 8, height = 6, units = "in", bg = "transparent")



# Plot total disagreement (NHD-MR) ----------------------------------------

plotdat <- allmrhr[!(allmrhr$Category=="Wet" & allmrhr$Month<8),]

plotdat$lab <- plotdat$mctypemr
plotdat$lab <- ifelse(plotdat$lab == "NHD dry Observation wet", "NHD intermittent,\nObservation wet", plotdat$lab)
plotdat$lab <- ifelse(plotdat$lab == "NHD wet Observation dry", "NHD perennial,\nObservation dry", plotdat$lab)

ggplot(plotdat, aes(lab)) +
  geom_bar(width=0.65) + 
  labs(x="", y="Observation count") +
  ggtitle("Disagreement with NHDPlus-MR V2") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"), axis.line = element_line(colour = "black"),
        plot.title=element_text(size=16, face="bold", margin=margin(t=0, r=0, b=20, l=0)),
        axis.title=element_text(size=14), axis.text=element_text(size=12), 
        plot.background = element_rect(fill = "transparent", color = NA), 
        axis.title.y = element_text(margin=margin(t=0, r=20, b=0, l=0)))

ggsave("C:/Users/khafe/Downloads/disagreement_mr.png", plot = last_plot(), width = 8, height = 6, units = "in", bg = "transparent")

# Plot misclassifications by month ----------------------------------------

plotdat <- allobs[!(allobs$Category=="Wet" & allobs$Month<8),]

ggplot(plotdat, aes(as.factor(Month))) + 
  geom_bar(aes(fill=mctype)) +
  scale_fill_manual(values=c("#08d104","#0c51fd","#fb0026")) +
  scale_x_discrete(breaks=0:12, labels=c("Not\nrecorded", "Jan", "Feb", "Mar", "Apr", 
                                         "May", "Jun", "Jul", "Aug", "Sep", "Oct", 
                                         "Nov", "Dec")) +
  labs(x="Month", y="Observation count", fill="Disagreement") +
  theme_bw() +
  theme(legend.position = c(0.98,0.98), legend.justification = c(1,1))

#Plot dimensions 700x375


# Plot log regression model results ---------------------------------------

moddat <- indat[!(indat$Category=="Wet" & indat$Month<8),]

moddat$pdsidif1 <- ifelse(moddat$pdsi_dif<0, moddat$pdsi_dif, 0)
moddat$pdsidif2 <- ifelse(moddat$pdsi_dif>=0, moddat$pdsi_dif, 0)

lr.spline.difint<- glm(mc ~ Category*pdsidif1 + Category*pdsidif2, data=moddat, family=binomial)

model.data <- augment(lr.spline.difint) %>% mutate(index = 1:n())
model.data$pdsi_dif <- model.data$pdsidif1 + model.data$pdsidif2
score <- qnorm((0.95/2) + 0.5)
model.data$lwr <- plogis(model.data$.fitted-score*model.data$.se.fit)
model.data$upr <- plogis(model.data$.fitted+score*model.data$.se.fit)
ggplot(model.data, aes(pdsi_dif, plogis(.fitted))) +
  geom_ribbon(aes(x=pdsi_dif, ymin=lwr, ymax=upr, group=Category), alpha = 0.2) + 
  geom_line(aes(color=Category), size=1.05) + 
  geom_point(aes(pdsi_dif, mc, colour=Category), alpha=0.1, size=2) +
  scale_color_manual(values=c("#fb0026", "#0c51fd")) +
  scale_y_continuous(name="Probability of disagreement", breaks=seq(0,1.0,0.2)) + 
  scale_x_continuous(name = "PDSI difference", breaks=seq(-8,8,2)) + 
  labs(color = "Observation\nType") +
  theme_bw() +
  theme(legend.position =c(0.98,0.52), legend.justification = c(1,1))

#Plot dimensions 700x400
