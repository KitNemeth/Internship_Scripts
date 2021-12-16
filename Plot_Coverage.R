#install.packages("data.table")
library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
dataM <- fread("MasterLandraceTeoInbredGBS_collapseDist0.02_TaxaSummary.txt")
out="Coverage.svg"
svg(out)

inbred = fread("AD_Inbred_Lines_No_BLANKS9.txt", header = FALSE)
landrace = fread("AllLandrace_No_BLANKS9.txt", header = FALSE)

inbred_indices = match(inbred$V1, dataM$`Taxa Name`, -1)

landrace_indices = match(landrace$V1, dataM$`Taxa Name`, -1)
Coverage1<-1-dataM$'Proportion Missing'[inbred_indices]
Coverage2<-1-dataM$'Proportion Missing'[landrace_indices]
# First distribution
hist(Coverage2, freq = T, breaks=50, col=rgb(250/255,159/255,0/255,0.5), xlab="Taxon Coverage", main="Length Coverage" )
  #Coverage1, breaks=50, ylim=c(0,2000), xlim=(0,1), col=rgb(1,0,0,0.5), xlab="Coverage", 
     #ylab="Frequency", main="Length Coverage" )
# Second with add=T to plot on top
hist(Coverage1, freq = T, breaks=50, col=rgb(204/255,121/255,167/255,0.5), add=T)
  #Coverage2, breaks=50, ylim=c(0,2000), col=rgb(0,0,1,0.5), add=T)

# Add legend
legend("topright", legend=c("Landraces","Inbreds"), col=c(rgb(250/255,159/255,0/255,0.5), 
                                                          rgb(204/255,121/255,167/255,0.5)), pt.cex=2, pch=15 )
dev.off()




