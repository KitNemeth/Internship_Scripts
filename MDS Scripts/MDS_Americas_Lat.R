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

Americas <- dataM[which(dataM$Population%in%c("Landrace - Ames Stock Center","Landrace - RIMMA","Landrace - NSS","Landrace - AndeanMiguel","Landrace - SEED") & dataM$Lat!=""  & dataM$Elev!=""),]
#Americas <- dataM[which(dataM$Population%in%c("Landrace - Ames Stock Center","Landrace - RIMMA","Landrace - NSS","Landrace - AndeanMiguel","Landrace - SEED","Teosinte") & dataM$Lat!=""  & dataM$Elev!=""),]

mds_Americas <- MDS(dist = allDist,info = Americas,group = "Lat",k = 2,main = "Latitude",heat = T, pdfFile = "MasterLandraceTeoInbredGBS_collapseDist0.02_Americas_MDS_Lat.svg")
