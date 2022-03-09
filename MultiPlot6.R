library(cowplot)
library(gridGraphics)
library("grid")
library("ggplotify")
out="Multiplot.svg"
svg(out,width=10,height=5)
#windows(width=8, height=8)
line = 1
cex = 1
side = 3
adj=-0.21
par(mfrow=c(1,3))

#plot1
hist(Americaswithteo$`Proportion Missing`, freq = T, ylim=c(0,800), xlim=c(0.2,1), col = "#c76674",  xlab="Proportion Missing", main= "All Samples")
mtext("a",font = 2, side=side, line=line, cex=cex, adj=adj)

hist(Americas$`Proportion Missing`, freq = T, ylim=c(0,800), xlim=c(0.2,1), col = "#9a9945", xlab="Proportion Missing", main= "Americas Accessions")
mtext("b",font = 2, side=side, line=line, cex=cex, adj=adj)

hist(andean$`Proportion Missing`, freq = T, col = "#8961b3", ylim=c(0,350), xlim=c(0.2,1), xlab="Proportion Missing", main= "South American Accessions")
mtext("c",font = 2, side=side, line=line, cex=cex, adj=adj)

dev.off()


