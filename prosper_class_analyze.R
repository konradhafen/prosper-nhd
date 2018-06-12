
# Do setup ----------------------------------------------------------------

rm(list=ls())
library(ggplot2)
library(reshape2)
library(tidyverse)

wd <- "E:/konrad/Projects/usgs/prosper-nhd/data/outputs/csv"
fn <- "nhd_mr_class_buf20.csv" #for all reaches
fn <- "nhd_mr_class_buf20_huc810.csv" #for huc10s with high disagreement
fn <- "nhd_crb_hr_split_perintap_buf20_fac_catout.csv"

indat <- read_csv(paste(wd,fn,sep="/"))
fn <- "nhd_hr_buf20_cat_maj.csv"
rawdat <- data.frame(read_csv(paste(wd, fn, sep="/")))
fn <- "E:/konrad/Projects/usgs/prosper-nhd/data/scpdsi/csv/scpdsi_crb_prosper.csv"
crbpdsi <- read_csv(fn)

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


# Subset to perennial/intermittnet only and classify overall --------------

pidat <- subset(indat, indat$FCODE==46006 | indat$FCODE==55800 | indat$FCODE==46003 | indat$FCODE == 46007)
pidat$nclass <- ifelse(pidat$FCODE==46006 | pidat$FCODE==55800, "Perennial", "Intermittent")

#classify reaches as perennial or intermittent based on pwet

#cutoff proportion for perennial
cutoff <- 0.75
pidat <- subset(pidat, !is.na(pidat$pwet))
pidat$pclass <- ifelse(pidat$pwet >= cutoff, "Wet", "Dry")
#run functions section first
pidat$dclass <- mapply(misclass_type, pidat$nclass, pidat$pclass)

#melt
pidat.melt <- melt(pidat, "id", c("nclass", "pclass", "dclass"))
pidat.melt <- melt(pidat)


# Different cutoff values -------------------------------------------------

pidat <- subset(pidat, !is.na(pidat$pwet))
nclass <- ifelse(pidat$FCODE==46006 | pidat$FCODE==55800, "Perennial", "Intermittent")
cutoffs <- seq (0.5, 1.0, 0.05)
df <- data.frame()
print(nrow(pidat))
print(length(pidat$pwet))
print(length(nclass))

for (cutoff in cutoffs)
{
  print(cutoff)
  pclass <- ifelse(pidat$pwet >= cutoff, "Wet", "Dry")
  dclass <- mapply(misclass_type, nclass, pclass)
  print(length(pclass))
  print(length(dclass))
  mc=ifelse(dclass=="Agree", 0, 1)
  df <- rbind(df, data.frame(cutoff=cutoff, mc=(sum(mc, na.rm=T)/length(mc)), 
                             nwpd=sum(dclass == "NHD wet PROSPER dry")/length(dclass), 
                             ndpw=sum(dclass == "NHD dry PROSPER wet")/length(dclass)))
  print(df)
}


# Plot results ------------------------------------------------------------

plot(df$cutoff, df$mc, type="l", xlab="Perennial cutoff value", ylab="Proportion of disagreeing reaches", 
     ylim=c(0, 0.5))
lines(df$cutoff, df$nwpd, col="red")
lines(df$cutoff, df$ndpw, col="blue")
legend("topright", legend=c("Total disagreement", "NHD perennial PROSPER dry", "NHD intermittent PROSPER wet"),
       lwd = c(2,2,2),
       col = c("black", "red", "blue"))

# Save to csv -------------------------------------------------------------

write_csv(pidat, paste(wd,"nhd_hr_buf20_cat_distype.csv",sep="/"))


# Disagreement by year ----------------------------------------------------
namecols <- names(rawdat)

for (i in 2:14)
{
  rawdat[paste(namecols[i], "cls", sep="_")] <- mapply(misclass_type_cat, rawdat$FCODE, rawdat[,i])
  print (paste(i, "done"))
}


# Data for plot of misclass by year ---------------------------------------

agree <- colSums(rawdat[21:33] == "Agree")/nrow(rawdat)
prdry <- colSums(rawdat[21:33] == "NHD wet PROSPER dry")/nrow(rawdat)
prwet <- colSums(rawdat[21:33] == "NHD dry PROSPER wet")/nrow(rawdat)
years <- seq(2004, 2016)

andat <- data.frame(year=years, agree=agree, prdry=prdry, prwet=prwet)
andat.melt <- melt(andat, "year")
andat$scpdsi <- crbpdsi$scpdsi
write_csv(andat, "E:/konrad/Projects/usgs/prosper-nhd/data/outputs/csv/disagreement_with_scpdsi.csv")


# Plot of misclass type by year -------------------------------------------


plot(years, agree, ylim=c(0,1), pch=NA, xlab="Year", ylab="Proportion of NHD Reaches")
lines(years, agree, col="gray")
lines(years, prdry, col="red")
lines(years, prwet, col="blue")


# Plot of miscalss type by year (ggplot) ----------------------------------

ggplot(data=andat.melt, aes(x=year, y=value, group=variable, color=variable)) + 
  geom_line() + 
  geom_point() +
  scale_color_manual("", values=c("black", "red", "blue"), labels=c("Agree", "NHD wet PROSPER dry", 
                     "NHD dry PROSPER wet")) + 
  xlab("Year") + 
  ylab("Proportion of Reaches") + 
  scale_y_continuous(limits=c(0.0, 0.7), breaks = seq(0.0, 0.7, 0.1)) +
  scale_x_continuous(minor_breaks=seq(2004, 2016, 1))


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

