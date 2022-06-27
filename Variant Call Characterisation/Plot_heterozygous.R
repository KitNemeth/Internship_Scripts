#install.packages("data.table")
library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
dataM <- fread("MasterLandraceTeoInbredGBS_collapseDist0.02_TaxaSummary.txt")
out="Heterozygosity.svg"
svg(out)

inbred = fread("AD_Inbred_Lines_No_BLANKS9.txt", header = FALSE)
landrace = fread("AllLandrace_No_BLANKS9.txt", header = FALSE)



inbred_indices = match(inbred$V1, dataM$`Taxa Name`, -1)

landrace_indices = match(landrace$V1, dataM$`Taxa Name`, -1)
# First distribution
hist(dataM$`Proportion Heterozygous`[landrace_indices], freq = T, breaks=40, col=rgb(250/255,159/255,0/255,0.5), xlab="Proportion Heterozygous", main="Heterozygosity" )

# Second with add=T to plot on top
hist(dataM$`Proportion Heterozygous`[inbred_indices], freq = T, breaks=40, col=rgb(204/255,121/255,167/255,0.5), add=T)

# Add legend
legend("topright", legend=c("Landraces","Inbreds"), col=c(rgb(250/255,159/255,0/255,0.5), 
                                                      rgb(204/255,121/255,167/255,0.5)), pt.cex=2, pch=15 )
dev.off()
