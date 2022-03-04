library(cowplot)
library(gridGraphics)
library("grid")
library("ggplotify")
out="Multiplot.svg"
svg(out)
#windows(width=8, height=8)
line = 1
cex = 1
side = 3
adj=-0.23
par(mfrow=c(2,2))

#plot1
hist(Americaswithteo$`Proportion Missing`, freq = F, col = "#c76674",  xlab="Proportion Missing", main= "All Zea")
mtext("a",font = 2, side=side, line=line, cex=cex, adj=adj)

hist(Americas$`Proportion Missing`, freq = F, col = "#9a9945", xlab="Proportion Missing", main= "American Landraces")
mtext("b",font = 2, side=side, line=line, cex=cex, adj=adj)

hist(andean$`Proportion Missing`, freq = F, col = "#8961b3", xlab="Proportion Missing", main= "South American Landraces")
mtext("c",font = 2, side=side, line=line, cex=cex, adj=adj)



dev.off()


