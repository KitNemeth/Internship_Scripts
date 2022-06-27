library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
dataM <- fread("MasterLandraceTeoInbredGBS_collapseDist0.02_AllLandrace_SiteSummary.txt")
dataN <- fread("MasterLandraceTeoInbredGBS_collapseDist0.02_AD_Inbred_Lines_SiteSummary.txt")
Ixos<-dataM$'Minor Allele Frequency'
Primadur<-dataN$'Minor Allele Frequency'
out="Minor_Allele_Frequency.svg"
svg(out)
# First distribution
hist(Ixos, breaks=50, xlim=c(0,0.5), ylim=c(0,600000), col=rgb(250/255,159/255,0/255,0.5), xlab="Minor Allele Frequency", 
     ylab="Frequency", main="Minor Allele Frequency" )

# Second with add=T to plot on top
hist(Primadur, breaks=50, xlim=c(0,0.5), ylim=c(0,600000), col=rgb(204/255,121/255,167/255,0.5), add=T)

# Add legend
legend("topright", legend=c("Landraces","Inbreds"), col=c(rgb(250/255,159/255,0/255,0.5), 
                                                          rgb(204/255,121/255,167/255,0.5)), pt.cex=2, pch=15 )
dev.off()