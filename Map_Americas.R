#install.packages("data.table")
library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
source("L:/Krisztian/TRGFunctions.R")
#dataM <- read.table("MasterLandraceTeoInbredGBS_collapseDist0.02_forMDS.txt",header=T,as.is = T,sep = "\t")
dataM <- as.data.frame(fread("MasterLandraceTeoInbredGBS_collapseDist0.02_forMDS.txt"))
str(dataM)
CIMMYT_12S_13S_RIMMA_Teosinte <- dataM[which(dataM$Population%in%c("Landrace","Landrace - Ames Stock Center","Teosinte")),]
#out="CIMMYT_12S_13S_RIMMA_Teosinte.svg"
#svg(out)
MapFromDF(wgs = CIMMYT_12S_13S_RIMMA_Teosinte,yCol = "Lat",xCol = "Long",labelCol = "Pedigree",plotname = "CIMMYT 12S 13S RIMMA_Teosinte",elevation = T,labels = F,ptCol = "darkorchid1",colorCol="Geographic_color",resolutionMeters=100,ptSize = 1,pdfFile = "CIMMYT_12S_13S_RIMMA_Teosinte.svg")
#dev.off()
