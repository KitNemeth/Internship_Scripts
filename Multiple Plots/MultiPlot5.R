library(cowplot)
library(gridGraphics)
library("grid")
library("ggplotify")
source("L:/Krisztian/Scripts/TRGFunctionsv3.R")
out="MDSmultiplot.svg"
svg(out,width=22,height=8.5)

line = 1
cex = 2
side = 3
adj=-0.085
par(mfrow=c(1,2))

mds_Americas <- MDS(dist = allDist,info = Americas,group = "Country",k = 2,main = "Americas Accessions", cex.main = 2)
mtext("a",font = 2, side=side, line=line, cex=cex, adj=adj)

mds_andean <- MDS(dist = allDist,info = andean,group = "Country",k = 2,main = "South American Accessions", cex.main = 2)
mtext("b",font = 2, side=side, line=line, cex=cex, adj=adj)

dev.off()