library(cowplot)
library(gridGraphics)
library("grid")
library("ggplotify")
source("L:/Krisztian/TRGFunctions_original.R")
out="Americas_Maps.svg"
svg(out)

line = 1
cex = 1
side = 3
adj=-0.2
#par(mfrow=c(1,1))
#dataM <- as.data.frame(fread("MasterLandraceTeoInbredGBS_collapseDist0.02_ForMDSv2Test.txt"))
#taxalist <- read.table(file("AllLandrace_covfilt0.1_taxalist.txt"),skip=1,sep="\t")
#taxalist <- as.vector(t(as.matrix(taxalist)))
#CIMMYT_12S_13S_RIMMA_Teosinte <- dataM[which(dataM$Taxa%in%taxalist),]
dev.new(width=5, height=5, unit="cm")
par(mfrow=c(1,2))
par(cex=0.7)
MapFromDF(wgs = CIMMYT_12S_13S_RIMMA_Teosinte,yCol = "Lat",xCol = "Long",labelCol = "Pedigree", plotname = NULL,elevation = T,labels = F,ptCol = "darkorchid1",colorCol="Seed_Bank_color",resolutionMeters=100, ptSize = 0.6)
par(cex=0.7)
legend("bottomleft", inset=c(0,0), legend=c("Swarts 2017","Sanchez","CIMMYT","RIMMA","SEED"), pch=21, col="black", pt.bg=c("#ba6437", "#98a441", "#7f63b8", "#b94b75", "#50b47b"),)
#mtext("A")
mtext("a",font = 2, side=side, line=line, cex=cex, adj=adj)

#dataM <- as.data.frame(fread("MasterLandraceTeoInbredGBS_collapseDist0.02_forMDSv2Test.txt"))
#ks_andes <- dataM[which(dataM$Population%in%c("Landrace - AndeanMiguel")),]
out="SouthAmericas_Map.svg"
svg(out)
par(cex=0.7)
MapFromDF(wgs = ks_andes,yCol = "Lat",xCol = "Long",labelCol = "Pedigree", plotname = NULL,elevation = T,labels = F,ptCol = "darkorchid1",colorCol="Geographic_color",resolutionMeters=100, ptSize = 1)
par(cex=0.7)
legend("bottomleft", inset=c(0,0), legend=c("<1700m",">1700m"), pch=21, col="black", pt.bg=c("#98823c", "#9a5ea1"),)

mtext("b",font = 2, side=side, line=line, cex=cex, adj=adj)

dev.off()

