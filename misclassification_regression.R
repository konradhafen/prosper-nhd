
# Do setup ----------------------------------------------------------------

rm(list=ls())
library(tidyverse)

wd <- "E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\outputs\\csv"
fn <- "nhd_crb_hr_split_perintap_buf20.csv"

indat <- read_csv(fn)


# Define functions --------------------------------------------------------

misclass_type_cat <- function(fcode, catval)
{
  if (((fcode == 46006 | fcode==55800) & catval > 0) | ((fcode != 46006 | fcode != 55800) & catval < 0))
  {
    return ("Agree")
  }
  else if ((fcode == 46006 | fcode==55800) & catval < 0)
  {
    return ("NHD wet PROSPER dry")
  }
  else if ((fcode != 46006 | fcode != 55800) & catval > 0)
  {
    return("NHD dry PROSPER wet")
  }
  else
  {
    return ("Invalid")
  }
}

# Subset and calculate misclassifications ---------------------------------

keeps <- c("id", "FCODE", "X16_maj", "X16_mean")
wrkdat <- indat[, keeps]

wrkdat$mc_type <- misclass_type_cat(wrkdat$FCODE, wrkdat$X16_maj)
wrkdat <- subset(wrkdat, wrkdat$misclass != "Invalid")
wrkdat$mc <- ifelse(wrkdat$mc_type == "Agree", 0, 1)


# Logistic regression -----------------------------------------------------

logmodel <- glm(mc ~ X16_maj, data=wrkdat, family=binomial(link="logit"))


