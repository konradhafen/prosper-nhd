
# Do setup ----------------------------------------------------------------

rm(list=ls())

fn = "E:/konrad/Projects/usgs/prosper-nhd/data/outputs/csv/disagreement_with_scpdsi.csv"

figdat <- read.csv(fn)

# Plot misclassification with scPDSI --------------------------------------


dev.off()

width = 7.5
height = 7.5
units = 'in'
pointsize = 12
res = 600
bg="white"
filename = "crb_hr_prosper_disagree_with_scpdsi.png"
setwd("E:/konrad/Projects/usgs/prosper-nhd/figs")
png(filename=filename, width=width, height=height, units=units, pointsize=pointsize, bg=bg, res=res)


par(oma = c(0,0,0,0) + 0.1, mar = c(4,4,2,4) + 0.1)


ymax <- 0.7
x1 = 2004
x2 = 2016
xlim = c(x1,x2)
ylim1 = c(0.0, 0.7)
ylim2 = c(-2.5, 2.5)

plot(figdat$year, figdat$agree, type="l", xlim = xlim, col="gray", lwd=1,
     ylab = "Proportion of reaches", xlab = "Water Year", main = "Columbia River Basin",
     ylim = ylim1)
lines(figdat$year, figdat$prdry, col="red")
lines(figdat$year, figdat$prwet, col="blue")

par(new=T)
plot(figdat$year, figdat$scpdsi, axes=F, xlab=NA, ylab=NA, type="l")
axis(side=4)
mtext(side=4, "scPDSI")
#x.poly <- c(datBasin$watYear, datBasin$watYear[nrow(datBasin)], datBasin$watYear[1])         # Adjoin two x-coordinates

datPoly <-datBasin
datPoly[,basinnames[i]] <- ifelse(datPoly[,basinnames[i]]>0, datPoly[,basinnames[i]], 0)
ypos.poly <- c(datPoly[,basinnames[i]], min(datPoly[,basinnames[i]]), min(datPoly[,basinnames[i]]))                     # .. and the corresponding y-coordinates
ypos.poly <- c(datPoly[,basinnames[i]], 0, 0)
polygon(x.poly, ypos.poly, col=rgb(30/255,144/255,1,alpha = 0.35), 
        border=rgb(30/255,144/255,1,alpha = 0.8), lwd = 0.5)

datPoly <-datBasin
datPoly[,basinnames[i]] <- ifelse(datPoly[,basinnames[i]]<0, datPoly[,basinnames[i]], 0)
yneg.poly <- c(datPoly[,basinnames[i]], min(datPoly[,basinnames[i]]), min(datPoly[,basinnames[i]]))                     # .. and the corresponding y-coordinates
yneg.poly <- c(datPoly[,basinnames[i]], 0, 0)
polygon(x.poly, yneg.poly, col=rgb(220/255,20/255,60/255,alpha = 0.35), 
        border=rgb(220/255,20/255,60/255,alpha = 0.8), lwd = 0.5)
lines(datBasin$watYear, rep(0, nrow(datBasin)), lty=1, col = "gray")

if (i==1)
{
  legend("topleft", legend = c("Topo maps field-checked", "Field Observations collected"),
         fill = c("black", "gray40"),
         border = c(NA,NA),
         cex=0.8)
}


dev.off()

setwd("C:\\konrad\\USGS\\PROSER_NHD\\data\\FocusBasins\\csv")