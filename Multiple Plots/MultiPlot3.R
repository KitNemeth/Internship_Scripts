library(cowplot)
library(gridGraphics)
library("grid")
library("ggplotify")
source("L:/Krisztian/Scripts/TRGFunctions.R")
out="MDSmultiplot.svg"
svg(out,width=10,height=15)

line = 1
cex = 1
side = 3
adj=-0.13
par(mfrow=c(4,2))

mds_Americas <- MDS(dist = allDist,info = Americas,group = "combinedLatLong",k = 2,main = "American Landraces", cex.main = 1)
legend("bottomright", inset=c(0,0), legend=c("Latitude","Longitude"), pch=19, cex=0.7, col=c("red", "green"),)
mtext("a",font = 2, side=side, line=line, cex=cex, adj=adj)

mds_andean <- MDS(dist = allDist,info = andean,group = "combinedLatLong",k = 2,main = "South American Landraces", cex.main = 1)
legend("bottomright", inset=c(0,0), legend=c("Latitude","Longitude"), pch=19, cex=0.7, col=c("red", "green"),)
mtext("b",font = 2, side=side, line=line, cex=cex, adj=adj)

mds_Americas <- MDS(dist = allDist,info = Americas,group = "combinedGeo",k = 2,main = "")
legend("bottomright", inset=c(0,0), legend=c("Latitude","Longitude", "Elevation"), pch=19, cex=0.7, col=c("red", "green", "blue"),)
mtext("c",font = 2, side=side, line=line, cex=cex, adj=adj)

mds_andean <- MDS(dist = allDist,info = andean,group = "combinedGeo",k = 2,main = "")
legend("bottomright", inset=c(0,0), legend=c("Latitude","Longitude", "Elevation"), pch=19, cex=0.7, col=c("red", "green", "blue"),)
mtext("d",font = 2, side=side, line=line, cex=cex, adj=adj)

Americas <-Americas[order(Americas$Beck_KG_V1),]
mds_Americas <- MDS(dist = allDist,info = Americas,group = "Beck",k = 2,main = "")
mtext("e",font = 2, side=side, line=line, cex=cex, adj=adj)

andean  <-andean[order(andean$Beck_KG_V1),]
mds_andean <- MDS(dist = allDist,info = andean,group = "Beck",k = 2,main = "")
mtext("f",font = 2, side=side, line=line, cex=cex, adj=adj)


dev.off()

