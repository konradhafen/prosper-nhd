# Do setup ----------------------------------------------------------------

rm(list=ls())

fn = "E:/konrad/Projects/usgs/prosper-nhd/data/quads/csv/quads_crb_scpdsi_ppt.csv"

figdat <- read.csv(fn)


# Histogram of check year -------------------------------------------------

hist(figdat$chck_year, breaks=seq(min(figdat$chck_year), max(figdat$chck_year), 1), 
     main="Quad Field Check Year", col="gray50", border=NA, xlab="Year")

# Histogram of scpdsi -----------------------------------------------------

hist(figdat$pdsi_mean, breaks=seq(-8, 8, 0.25),
     main="scPDSI During Field Check Water Year", col="gray50", border=NA, xlab="scPDSI")
