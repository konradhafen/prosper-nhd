
# Do setup ----------------------------------------------------------------

rm(list=ls())
library(dplyr)
library(reshape2)
library(ggplot2)

wd <- "E:/konrad/Projects/usgs/prosper-nhd/data/method_dev/csv"
fn <- "flowline_nd-025-095.csv"
# fn <- "flowline-099-099.csv"
# fn <- "flowline_base.csv"
fn <- "flowline_compare.csv"

indat <- read.csv(paste(wd, fn,sep="/"))
indat <- filter_all(indat, all_vars(. != -9999))

# Quick check -------------------------------------------------------------

orderdat <- indat[order(indat$count),]


# Anova of counts ---------------------------------------------------------

keepcols <- grepl("count", names(indat))
count.df <- indat[,keepcols]
count.df.melt <- melt(count.df)
ct.aov <- aov(value ~ variable, data=count.df.melt)
summary(ct.aov)
TukeyHSD(ct.aov)


# Anova of means ----------------------------------------------------------

keepcols <- grepl("mean", names(indat))
keepcols[grep("LENGTHKM", colnames(indat))] <- TRUE
meanaov.df <- indat[,keepcols]
meanaov.df.melt <- melt(meanaov.df, id.vars = "LENGTHKM")
mean.aov <- aov(value ~ variable*LENGTHKM, data=meanaov.df.melt)
summary(mean.aov)
TukeyHSD(mean.aov)

# Anova of medians --------------------------------------------------------

keepcols <- grepl("med", names(indat))
medaov.df <- indat[,keepcols]
medaov.df.melt <- melt(medaov.df)
med.aov <- aov(value ~ variable, data=medaov.df.melt)
summary(med.aov)
TukeyHSD(med.aov)


# Anova of standard deviation ---------------------------------------------

keepcols <- grepl("sd", names(indat))
sdaov.df <- indat[,keepcols]
sdaov.df.melt <- melt(sdaov.df)
sd.aov <- aov(value ~ variable, data=sdaov.df.melt)
summary(sd.aov)
TukeyHSD(sd.aov)

# Difference in counts between catseed and buffer sizes -------------------

diff.df <- data.frame(gc=indat$GRIDCODE, length=indat$LENGTHKM, count=indat$count)
for (i in seq(20,100,20))
{
  varname <- paste("count",i,sep="")
  newname <- paste("dif",i,sep="")
  pername <- paste("per",i,sep="")
  diff.df[varname] <- indat[varname]
  diff.df[newname] <- indat[varname] - indat$count
  diff.df[pername] <- diff.df[newname]/diff.df$count
}

plot(diff.df$count, diff.df$per20)
plot(diff.df$count, diff.df$count20)


# Difference in means -----------------------------------------------------

mean.df <- data.frame(gc=indat$GRIDCODE, length=indat$LENGTHKM, mean=indat$mean)
for (i in seq(20,100,20))
{
  varname <- paste("mean",i,sep="")
  newname <- paste("dif",i,sep="")
  pername <- paste("per",i,sep="")
  mean.df[newname] <- indat[varname] - indat$mean
  mean.df[pername] <- mean.df[newname]/mean.df$mean
}

mean.lm <- lm((per20) ~ length, data=mean.df)
summary(mean.lm)
plot(mean.df$length, (mean.df$per20))
abline(mean.lm$coefficients[1], mean.lm$coefficients[2])

# difference in sd --------------------------------------------------------

sd.df <- data.frame(gc=indat$GRIDCODE, length=indat$LENGTHKM, sd=indat$sd)
for (i in seq(20,100,20))
{
  varname <- paste("sd",i,sep="")
  newname <- paste("dif",i,sep="")
  pername <- paste("per",i,sep="")
  sd.df[newname] <- indat[varname] - indat$sd
  sd.df[pername] <- sd.df[newname]/sd.df$sd
}

# Prelim analysis ---------------------------------------------------------

hist(diff.df$dif20)
hist(diff.df$dif40)
hist(diff.df$dif60)
hist(diff.df$dif80)
hist(diff.df$dif100)


# Compare raw buffer to thresholded buffer --------------------------------

#use filename flowline_compare.csv
indat$ctdif <- indat$count-indat$countd
indat$dif <- ifelse(indat$maj != indat$majd, 1, 0)
indat$permdif <- ifelse((indat$maj > 0 & indat$majd < 0) | (indat$maj < 0 & indat$majd > 0), 1, 0)
