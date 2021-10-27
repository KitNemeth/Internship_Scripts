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

MexSA <- dataM[which(dataM$Geographic%in%c("Andean","SA_Low", "CA_Low","CA_High","NorthMexican","C_Mex_Low", "C_Mex_High") & dataM$Lat!=""  & dataM$Elev!=""),]

mds_MexSA <- MDS(dist = allDist,info = MexSA,group = "combinedGeo",k = 2,main = "Central and South American Accessions",pdfFile = "MasterLandraceTeoInbredGBS_collapseDist0.02_CSA_MDS_Geo.svg")
