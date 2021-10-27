#install.packages("data.table")
library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
dataM <- fread("MasterLandraceTeoInbredGBS_collapseDist0.02_TaxaSummary.txt")
out="Coverage.svg"
svg(out)

inbred = fread("AD_Inbred_Lines_No_BLANKS8.txt", header = FALSE)
landrace = fread("AllLandrace_No_BLANKS8.txt")

inbred_indices = match(inbred$V1, dataM$`Taxa Name`, -1)

landrace_indices = match(landrace$`Taxa Name`, dataM$`Taxa Name`, -1)
Coverage1<-1-dataM$'Proportion Missing'[inbred_indices]
Coverage2<-1-dataM$'Proportion Missing'[landrace_indices]
# First distribution
hist(Coverage2, ylim=c(0,6), freq = F, breaks=50, col=rgb(1,0,0,0.5), xlab="Length Coverage", main="Length Coverage" )
  #Coverage1, breaks=50, ylim=c(0,2000), xlim=(0,1), col=rgb(1,0,0,0.5), xlab="Coverage", 
     #ylab="Frequency", main="Length Coverage" )
# Second with add=T to plot on top
hist(Coverage1, freq = F, breaks=50, col=rgb(0,0,1,0.5), add=T)
  #Coverage2, breaks=50, ylim=c(0,2000), col=rgb(0,0,1,0.5), add=T)

# Add legend
legend("topright", legend=c("Landraces","Inbreds"), col=c(rgb(1,0,0,0.5), 
                                                          rgb(0,0,1,0.5)), pt.cex=2, pch=15 )
dev.off()




