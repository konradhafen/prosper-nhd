
# Do setup ----------------------------------------------------------------

rm(list=ls())
library(tidyverse)

wd <- "E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\outputs\\csv"
setwd(wd)
fn <- "nhd_crb_hr_split_perintap_buf20_fac.csv"

indat <- read_csv(fn)


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
indat <- subset(indat, indat$fac_mean < 5000000) 

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

logmodel <- glm(mc ~ abs(dif_mean), data=meltdat, family="binomial")
logmodel2 <- glm(mc ~ abs(dif_mean + as.factor(year)), data=meltdat, family="binomial")
logmodel3 <- glm(mc ~ abs(dif_mean) + fac_mean, data=meltdat, family="binomial")


# Save csv ----------------------------------------------------------------

write_csv(wrkdat, "nhd_crb_hr_split_perintap_out.csv")

