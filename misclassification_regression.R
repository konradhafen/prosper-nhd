
# Do setup ----------------------------------------------------------------

rm(list=ls())
library(tidyverse)

wd <- "E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\outputs\\csv"
setwd(wd)
fn <- "nhd_crb_hr_split_perintap_buf20_fac.csv"

indat <- read_csv(fn)
huc8 <- read_csv("huc8_mask.csv")
exclude <- as.data.frame(huc8$HUC8_Long)
huc8s <- substr(indat$REACHCODE, 1, 8)
condition <- huc8s %in% as.character(exclude$`huc8$HUC8_Long`)
indat <- indat[!condition,]

# Define functions --------------------------------------------------------

misclass_type_cat <- function(fcode, catval)
{
  if ((fcode == "wet" & catval > 0) | (fcode != "wet" & catval < 0))
  {
    return ("Agree")
  }
  else if (fcode == "wet" & catval < 0)
  {
    return ("NHD wet PROSPER dry")
  }
  else if (fcode != "wet" & catval > 0)
  {
    return("NHD dry PROSPER wet")
  }
  else
  {
    return ("Invalid")
  }
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


# Melted data frame by year -----------------------------------------------

#subset input data based on drainage area
indat <- subset(indat, indat$count > 2) 

meltdat <- data.frame(id = numeric(),
                      FCODE = numeric(),
                      # fac_med = numeric(),
                      fac_mean = numeric(),
                      cat_cls = numeric(),
                      dif_mean = numeric(),
                      nhd_cls = character(),
                      # mc_type = character(),
                      mc = numeric(),
                      year = numeric())

for (i in 2004:2016)
{
  yearabv <- substrRight(as.character(i), 2)
  keeps <- c("id", "FCODE", "fac_med", "fac_mean", paste(yearabv, "_maj", sep=""),
             paste(yearabv, "_mean", sep=""))
  tempdat <- as.data.frame(indat[, keeps])
  names(tempdat) <- c("id", "FCODE", "fac_med", "fac_mean", "cat_cls", "dif_mean")
  tempdat <- as.data.frame(subset(tempdat, !is.na(tempdat$cat_cls)))
  tempdat <- subset(tempdat, abs(tempdat$cat_cls) >= 1)
  tempdat$nhd_cls <- ifelse(tempdat$FCODE == 46006 | tempdat$FCODE == 55800, "wet", "dry")
  tempdat$mc_type <- mapply(misclass_type_cat, tempdat$nhd_cls, tempdat$cat_cls)
  tempdat <- subset(tempdat, tempdat$mc_type != "Invalid")
  tempdat$mc <- ifelse(tempdat$mc_type == "Agree", 0, 1)
  tempdat$year <- i
  tempdat <- tempdat[, c("id", "FCODE", "fac_mean", "cat_cls", "nhd_cls", "dif_mean", "mc", "year")]
  meltdat <- rbind(meltdat, tempdat)
  print(nrow(meltdat))
}
rm(tempdat)
rm(indat)


# Subset based on scpdsi difference ---------------------------------------

meltdat.pdsi <- subset(meltdat, abs(meltdat$dif_mean) < 0.5)
meltdat.pdsi$adif_mean <- abs(meltdat.pdsi$dif_mean)

logr.dif <- glm(mc ~ adif_mean, data=meltdat.pdsi, family="binomial")
logr.year <- glm(mc ~ as.factor(year), data=meltdat.pdsi, family="binomial")
logr.cat <- glm(mc ~ as.factor(cat_cls), data=meltdat.pdsi, family="binomial")
logr.fac <- glm(mc ~ fac_mean, data=meltdat.pdsi, family="binomial")
logr.fcode <- glm(mc ~ as.factor(FCODE), data=meltdat.pdsi, family="binomial")

pred.fcode <- cbind(data.frame(FCODE=c(46003,46006,46007,55800)),
                    predict(logr.fcode, newdata=data.frame(FCODE=c(46003,46006,46007,55800)),
                            type="response"))


# Logistic regression -----------------------------------------------------

meltdat$adif_mean <- abs(meltdat$dif_mean)
logr.dif <- glm(mc ~ adif_mean, data=meltdat, family="binomial")
logr.fcode <- glm(mc ~ as.factor(FCODE), data=meltdat, family="binomial")
logr.diffcode <- glm(mc ~ adif_mean + as.factor(FCODE), data=meltdat, family="binomial")
logr.year <- glm(mc ~ year, data=meltdat, family="binomial")
logr.cat <- glm(mc ~ as.factor(cat_cls), data=meltdat, family="binomial")
logr.difyr <- glm(mc ~ adif_mean + as.factor(year), data=meltdat, family="binomial")
logr.fac <- glm(mc ~ fac_mean, data=meltdat, family="binomial")
logr.diffac <- glm(mc ~  adif_mean + fac_mean, data=meltdat, family="binomial")
logr.diffacyr <- glm(mc ~  adif_mean + fac_mean + as.factor(year), data=meltdat, family="binomial")



# Predict values scPDSI dif -----------------------------------------------

predvalues <- data.frame(adif_mean=seq(0,10,0.1))
predvalues <- cbind(predvalues, predict(logr.dif, newdata=predvalues, type='link', se=T))
plot(predvalues$adif_mean, predvalues$PredProb)

predvalues <- within(predvalues, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

plot(predvalues$adif_mean, predvalues$PredictedProb, type="l", xlab="scPDSI Difference", 
     ylab="Probability of Classification Difference")
lines(predvalues$adif_mean, predvalues$LL, col="red")
lines(predvalues$adif_mean, predvalues$UL, col="red")


# Predict values FAC ------------------------------------------------------

predvalues <-data.frame(fac_mean=seq(1,189000000,1000))
predvalues <- cbind(predvalues, predict(logr.fac, newdata=predvalues, type='link', se=T))

predvalues <- within(predvalues, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

plot(predvalues$fac_mean, predvalues$PredictedProb, type="l", xlab="Flow Accumulation", 
     ylab="Probability of Classification Difference")
lines(predvalues$fac_mean, predvalues$LL, col="red")
lines(predvalues$fac_mean, predvalues$UL, col="red")


# Predict value scPDSI dif and FAC ----------------------------------------

library(plotly)
predvalues <- data.frame(adif_mean=rep(seq(0,9.9,0.1), 18900), fac_mean=seq(1,1890000000,1000))
predvalues <- cbind(predvalues, predict(logr.diffac, 
                                        newdata=expand.grid(adif_mean=seq(0,9.9,0.1), 
                                                            fac_mean=c(100, 1000, 10000, 100000, 1000000, 10000000, 100000000)), 
                                        type='link', se=T))

predvalues <- within(predvalues, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

plotdat <- matrix(predict(logr.diffac, 
        newdata=expand.grid(adif_mean=seq(0,9.9,0.1), 
                            fac_mean=c(100, 1000, 10000, 100000, 1000000, 10000000, 100000000)),
        type='response'),
        nrow=length(seq(0,9.9,0.1)), 
        ncol=7)
        

x <- matrix(seq(0,9.9,0.1), nrow=length(seq(0,9.9,0.1)), ncol=1)
y <- matrix(c(100, 1000, 10000, 100000, 1000000, 10000000, 100000000), nrow=7, ncol=1)
p <- plot_ly(z= ~plotdat) %>% add_surface()

# Save csv ----------------------------------------------------------------

write_csv(wrkdat, "nhd_crb_hr_split_perintap_out.csv")

