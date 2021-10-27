#install.packages("data.table")
library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
dataM <- fread("MasterLandraceTeoInbredGBS_collapseDist0.02_TaxaSummary.txt")
out="Heterozygosity.svg"
svg(out)

inbred = fread("AD_Inbred_Lines_No_BLANKS8.txt", header = FALSE)
landrace = fread("AllLandrace_No_BLANKS8.txt")



inbred_indices = match(inbred$V1, dataM$`Taxa Name`, -1)

landrace_indices = match(landrace$`Taxa Name`, dataM$`Taxa Name`, -1)
# First distribution
hist(dataM$`Proportion Heterozygous`[landrace_indices], freq = F, breaks=50, col=rgb(1,0,0,0.5), ylim = c(0,250), xlab="Proportion Heterozygous", main="Heterozygosity" )

# Second with add=T to plot on top
hist(dataM$`Proportion Heterozygous`[inbred_indices], freq = F, breaks=50, col=rgb(0,0,1,0.5), add=T)

# Add legend
legend("topright", legend=c("Landraces","Inbreds"), col=c(rgb(1,0,0,0.5), 
                                                      rgb(0,0,1,0.5)), pt.cex=2, pch=15 )
dev.off()
