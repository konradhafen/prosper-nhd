
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
                      # FCODE = numeric(),
                      # fac_med = numeric(),
                      fac_mean = numeric(),
                      # cat_cls = numeric(),
                      dif_mean = numeric(),
                      # nhd_cls = character(),
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
  tempdat$nhd_cls <- ifelse(tempdat$FCODE == 46006 | tempdat$FCODE == 55800, "wet", "dry")
  tempdat$mc_type <- mapply(misclass_type_cat, tempdat$nhd_cls, tempdat$cat_cls)
  tempdat <- subset(tempdat, tempdat$mc_type != "Invalid")
  tempdat$mc <- ifelse(tempdat$mc_type == "Agree", 0, 1)
  tempdat$year <- i
  tempdat <- tempdat[, c("id", "fac_mean", "dif_mean", "mc", "year")]
  meltdat <- rbind(meltdat, tempdat)
  print(nrow(meltdat))
}
rm(tempdat)
rm(indat)

# Subset and calculate misclassifications ---------------------------------

keeps <- c("id", "FCODE", "16_maj", "16_mean")
wrkdat <- indat[, keeps]
names(wrkdat) <- c("id", "FCODE", "X16_maj", "X16_mean")
wrkdat <- as.data.frame(subset(wrkdat, !is.na(wrkdat$X16_maj)))
wrkdat$nhd_cls <- ifelse(wrkdat$FCODE == 46006 | wrkdat$FCODE == 55800, "wet", "dry")

wrkdat$mc_type <- mapply(misclass_type_cat, wrkdat$nhd_cls, wrkdat$X16_maj)
wrkdat <- subset(wrkdat, wrkdat$mc_type != "Invalid")
wrkdat$mc <- ifelse(wrkdat$mc_type == "Agree", 0, 1)


# Logistic regression -----------------------------------------------------

meltdat$adif_mean <- abs(meltdat$dif_mean)
logmodel <- glm(mc ~ adif_mean, data=meltdat, family="binomial")
logmod <- glm(mc ~ dif_mean, data=meltdat, family="binomial")
logr.year <- glm(mc ~ year, data=meltdat, family="binomial")
logmodel2 <- glm(mc ~ abs(dif_mean) + as.factor(year), data=meltdat, family="binomial")
logmodel3 <- glm(mc ~ abs(dif_mean) + fac_mean, data=meltdat, family="binomial")
logr.fac <- glm(mc ~ fac_mean, data=meltdat, family="binomial")


# Predict values ----------------------------------------------------------

predvalues <- data.frame(adif_mean=seq(0,10,0.1))
predvalues <- cbind(predvalues, predict(logmodel, newdata=predvalues, type='link', se=T))
plot(predvalues$adif_mean, predvalues$PredProb)

predvalues <- within(predvalues, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

plot(predvalues$adif_mean, predvalues$PredictedProb, type="l", xlab="scPDSI Difference", 
     ylab="Probability of Classification Difference", ylim=c(0.3,0.4))
lines(predvalues$adif_mean, predvalues$LL, col="red")
lines(predvalues$adif_mean, predvalues$UL, col="red")

# Save csv ----------------------------------------------------------------

write_csv(wrkdat, "nhd_crb_hr_split_perintap_out.csv")

