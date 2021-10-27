library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
source("L:/Krisztian/TRGFunctions_original.R")

distances <- as.data.frame(fread("MasterLandraceTeoInbredGBS_collapseDist0.02_IBS.dist.txt",sep="\t",skip=5,stringsAsFactors = F)) #hamming (IBS) distance matrix where only rownames are labelled (TASSEL format)
rownames(distances) <- distances$V1
distances <- distances[,2:ncol(distances)]
colnames(distances) <- rownames(distances)
allDist <- distances

dataM <- as.data.frame(fread("MasterLandraceTeoInbredGBS_collapseDist0.02_forMDSv2Test.txt"))
dataM <- dataM[which(dataM$`Proportion Missing`<.95),]
dataM$Lat = as.numeric(dataM$Lat)
dataM$Long = as.numeric(dataM$Long)
dataM$Elev = as.numeric(dataM$Elev)

ks_andes <- dataM[which(dataM$Population%in%c("Landrace - Ames Stock Center","Landrace - RIMMA","Landrace - NSS","Teosinte","Landrace - AndeanMiguel")),]
MapFromDF(wgs = ks_andes,yCol = "Lat",xCol = "Long",labelCol = "Pedigree", plotname = "With new Andean",elevation = T,labels = F,ptCol = "darkorchid1",colorCol="Beck_color",resolutionMeters=100, ptSize = 1, pdfFile = "KSPlusNew.svg")
mds_ks_andes <- MDS(dist = allDist,info = ks_andes,group = "Geographic",k = 2,main = "With new Andean",cex.main = 2, cex.lab = 1.5, cex.axis = 1.5, mar=c(5, 5, 4, 2), pdfFile = "MasterLandraceTeoInbredGBS_collapseDist0.02_ksPlusNew_MDS.svg")
