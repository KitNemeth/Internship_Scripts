#install.packages("data.table")
library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
source("L:/Krisztian/TRGFunctions_original.R")

distances <- as.data.frame(fread("MasterLandraceTeoInbredGBS_collapseDist0.02_IBS.dist.txt",sep="\t",skip=5,stringsAsFactors = F)) #hamming (IBS) distance matrix where only rownames are labelled (TASSEL format)
rownames(distances) <- distances$V1
distances <- distances[,2:ncol(distances)]
colnames(distances) <- rownames(distances)
allDist <- distances

DistFilt <- mds_andean$d

#dataM <- read.table("MasterLandraceTeoInbredGBS_collapseDist0.02_forMDS.txt",header=T,as.is = T,sep = "\t")
dataM <- as.data.frame(fread("MasterLandraceTeoInbredGBS_collapseDist0.02_forMDSv2Test.txt"))
dataM <- dataM[which(dataM$`Proportion Missing`<.95),]
dataM$Lat = as.numeric(dataM$Lat)
dataM$Long = as.numeric(dataM$Long)
dataM$Elev = as.numeric(dataM$Elev)
str(dataM)
#andean <- dataM[which(dataM$Geographic%in%c("Andean","SA_Low") & dataM$Accession!="Guatemala" & dataM$Lat!=""),]
#andean <- dataM[which(dataM$Lat<=13.45 & dataM$Long>=-90 & dataM$Lat!="" & dataM$Country!="COSTA RICA" & dataM$Country!="TRINIDAD AND TOBAGO" & dataM$Country!="GREDA" & dataM$Country!="SAINT VINCENT AND THE GREDINES" & dataM$Country!="PAMA" & dataM$Country!="NICARAGUA" & dataM$Country!="BARBADOS"),] 
andean <- dataM[which(dataM$Lat<=13.45 & dataM$Long>=-90 & dataM$Lat!="" & dataM$Country!="COSTA RICA" & dataM$Country!="TRINIDAD AND TOBAGO" & dataM$Country!="GREDA" & dataM$Country!="SAINT VINCENT AND THE GREDINES" & dataM$Country!="PAMA" & dataM$Country!="NICARAGUA" & dataM$Country!="BARBADOS"),]
#| dataM$TaxaNum==14257),]
#andean2 <- rbind(andean,  dataM[which(dataM$TaxaNum==14257),])

MapFromDF(wgs = andean,yCol = "Lat",xCol = "Long",labelCol = "Pedigree", plotname = "New South American accessions",elevation = T,labels = F,ptCol = "darkorchid1",colorCol="Geographic_color",resolutionMeters=100, ptSize = 1,pdfFile = "SouthAmericanAccession.svg")
#MapFromDF(wgs = andean,yCol = "Lat",xCol = "Long",labelCol = "Pedigree", plotname = "New South American accessions",elevation = T,labels = F,ptCol = "darkorchid1",colorCol="Seed_color",resolutionMeters=100, ptSize = 1,pdfFile = "SouthAmericanAccession.svg")
