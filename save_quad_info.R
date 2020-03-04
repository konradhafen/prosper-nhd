
# Load data ---------------------------------------------------------------

rm(list=ls())
options(scipen=999)

setwd("E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\data_release\\PRE")

fn = "quads_ppt_scpdsi_year.csv"

indat <- as.data.frame(read_csv(fn))


# Calculate survey year ---------------------------------------------------

indat$ClassYear <- ifelse(indat$Field_Chec > 0, indat$Field_Chec, indat$Survey_Yea)
indat <- indat[indat$ClassYear > 0,]

# Format final output -----------------------------------------------------

keeps <- c("US_7_ID", "ClassYear", "jpdsi_check", "jpdsi_per")

outdat <- indat[, keeps]

colnames(outdat) <- c("US_7_ID", "ClassYear", "pdsi", "ppt_per")

#outdat <- outdat[complete.cases(outdat$pdsi),]
#outdat <- outdat[complete.cases(outdat$ppt_per),]


# Print output file -------------------------------------------------------

setwd("E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\data_release\\FINAL")
write.csv(outdat, "quad_extent_data.csv", row.names=F)
