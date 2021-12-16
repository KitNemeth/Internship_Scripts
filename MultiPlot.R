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
hist(Coverage2, freq = F, breaks=50, col=rgb(250/255,159/255,0/255,0.5), xlab="Taxon Coverage", main= NULL )
#Coverage1, breaks=50, ylim=c(0,2000), xlim=(0,1), col=rgb(1,0,0,0.5), xlab="Coverage", 
#ylab="Frequency", main="Length Coverage" )
# Second with add=T to plot on top
hist(Coverage1, freq = F, breaks=50, col=rgb(204/255,121/255,167/255,0.5), add=T)
#Coverage2, breaks=50, ylim=c(0,2000), col=rgb(0,0,1,0.5), add=T)
mtext("a",font = 2, side=side, line=line, cex=cex, adj=adj)

# Add legend
legend("topright", legend=c("Landraces","Inbreds"), col=c(rgb(250/255,159/255,0/255,0.5), 
                                                          rgb(204/255,121/255,167/255,0.5)), pt.cex=2, pch=15 )
#plot2
hist(dataM$`Proportion Heterozygous`[landrace_indices], freq = F, ylim=c(0,200), breaks=40, col=rgb(250/255,159/255,0/255,0.5), xlab="Proportion Heterozygous", main= NULL)

#with add=T to plot on top
hist(dataM$`Proportion Heterozygous`[inbred_indices], freq = F, breaks=40, col=rgb(204/255,121/255,167/255,0.5), add=T)
mtext("b",font = 2, side=side, line=line, cex=cex, adj=adj)

# Add legend
legend("topright", legend=c("Landraces","Inbreds"), col=c(rgb(250/255,159/255,0/255,0.5), 
                                                          rgb(204/255,121/255,167/255,0.5)), pt.cex=2, pch=15 )
hist(Ixos, breaks=50, xlim=c(0,0.5), ylim=c(0,600000), col=rgb(250/255,159/255,0/255,0.5), xlab="Minor Allele Frequency", 
          ylab="Frequency", main=NULL)

# Second with add=T to plot on top
hist(Primadur, breaks=50, xlim=c(0,0.5), ylim=c(0,600000), col=rgb(204/255,121/255,167/255,0.5), add=T)

mtext("c",font = 2, side=side, line=line, cex=cex, adj=adj)

# Add legend
legend("topright", legend=c("Landraces","Inbreds"), col=c(rgb(250/255,159/255,0/255,0.5), 
                                                          rgb(204/255,121/255,167/255,0.5)), pt.cex=2, pch=15 )
plot(x,y,
          pch=20,
          cex=0.2, 
          col="Black",
          xlab="Coverage", ylab="Heterozygousity",
          main=NULL,)
abline(reg = lm(y ~ x),col = "blue",)
mtext("d",font = 2, side=side, line=line, cex=cex, adj=adj)


dev.off()


