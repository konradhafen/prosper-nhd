
# Do setup ----------------------------------------------------------------

rm(list=ls())
library(ggplot2)
library(reshape2)
library(tidyverse)

wd <- "E:/konrad/Projects/usgs/prosper-nhd/data/outputs/csv"
fn <- "nhd_mr_class_buf20.csv" #for all reaches
fn <- "nhd_mr_class_buf20_huc810.csv" #for huc10s with high disagreement
fn <- "nhd_hr_buf20_cat_fcode.csv"

indat <- read_csv(paste(wd,fn,sep="/"))

# functions ---------------------------------------------------------------

misclass_type <- function(nhdclass, prosperclass)
{
  if ((nhdclass == "Perennial" & prosperclass=="Wet") | (nhdclass == "Intermittent" & prosperclass=="Dry"))
  {
    return ("Agree")
  }
  else if ((nhdclass == "Perennial" & prosperclass=="Dry"))
  {
    return ("NHD wet PROSPER dry")
  }
  else if ((nhdclass == "Intermittent" & prosperclass=="Wet"))
  {
    return("NHD dry PROSPER wet")
  }
}


# Subset to perennial/intermittnet only and classify overall --------------

pidat <- subset(indat, indat$FCODE==46006 | indat$FCODE==46003)
pidat$nclass <- ifelse(pidat$FCODE==46006, "Perennial", "Intermittent")

#classify reaches as perennial or intermittent based on pwet

#cutoff proportion for perennial
cutoff <- 0.75
pidat$pclass <- ifelse(pidat$pwet >= cutoff, "Wet", "Dry")
#run functions section first
pidat$dclass <- mapply(misclass_type, pidat$nclass, pidat$pclass)

#melt
pidat.melt <- melt(pidat, "ID", c("nclass", "pclass", "dclass"))
pidat.melt <- melt(pidat)


# Save to csv -------------------------------------------------------------

write_csv(pidat, paste(wd,"nhd_hr_buf20_cat_distype.csv",sep="/"))

# Bar plot of misclassification types -------------------------------------

ggplot(pidat.melt) + 
  stat_count(mapping = aes(x=dclass, y=..prop.., group=1)) +
  labs(x="Disagreement type", y="Proportion of disagreement", title="Disagreement between PROSPER and HR-NHD")


# Bar plot of miscalssification by NHD type -------------------------------

ggplot(pidat) + 
  stat_count(mapping = aes(x=dclass, y=..prop.., group=1)) +
  facet_wrap(~nclass, ncol=1) + 
  labs(x="Disagreement type", y="Proportion of disagreement", title="Disagreement by HR-NHD classification")


# Bary plot of misclassification by PROSPER type --------------------------

ggplot(pidat) + 
  stat_count(mapping = aes(x=dclass, y=..prop.., group=1)) +
  facet_wrap(~pclass, ncol=1) + 
  labs(x="Disagreement type", y="Proportion of disagreement", title="Disagreement by PROSPER classification (HR-NHD)")


# Plot of classification changes vs drainage area -------------------------

ggplot(pidat, aes(x=fac_DA, y=ctswitch)) + 
  geom_point(aes(colour=pwet)) +
  scale_color_gradient(low="white", high="blue")

# Plot of classification changes vs drainage area -------------------------

ggplot(pidat, aes(x=fac_DA, y=ctswitch)) + 
  geom_point() + 
  facet_wrap(~nclass, ncol=1)


# Boxplot perennial vs intermittent ---------------------------------------

ggplot(pidat, aes(nclass, pwet)) +
  geom_boxplot() + 
  facet_wrap(~NAME_2, ncol=4)

