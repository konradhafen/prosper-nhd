
# Do setup ----------------------------------------------------------------

rm(list=ls())
library(dplyr)
library(reshape2)

wd <- "E:/konrad/Projects/usgs/prosper-nhd/data/method_dev/csv"
fn <- "flowline-095-090.csv"

indat <- read.csv(paste(wd, fn,sep="/"))
indat <- filter_all(indat, all_vars(. != -9999))

# Quick check -------------------------------------------------------------

orderdat <- indat[order(indat$count),]


# Get count columns -------------------------------------------------------

keepcols <- grepl("count", names(indat))
count.df <- indat[,keepcols]
count.df.melt <- melt(count.df)


# Anova of counts ---------------------------------------------------------

ct.aov <- aov(value ~ variable, data=count.df.melt)
summary(ct.aov)
TukeyHSD(ct.aov)

# Difference in counts between catseed and buffer sizes -------------------

diff.df <- data.frame(gc=indat$GRIDCODE, count=indat$count)
for (i in seq(20,100,20))
{
  varname <- paste("count",i,sep="")
  newname <- paste("dif",i,sep="")
  pername <- paste("per",i,sep="")
  diff.df[newname] <- indat[varname] - indat$count
  diff.df[pername] <- diff.df[newname]/diff.df$count
}

# Prelim analysis ---------------------------------------------------------

hist(diff.df$dif20)
hist(diff.df$dif40)
hist(diff.df$dif60)
hist(diff.df$dif80)
hist(diff.df$dif100)
