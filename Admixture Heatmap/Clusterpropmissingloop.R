library(data.table)
library("writexl")
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
Clustered_data <- as.data.frame(fread("All_landrace_pheatmap_clusters.txt"))

#Taxasum <- as.data.frame(fread("MasterLandraceTeoInbredGBS_collapseDist0.02_collapseSEED_TaxaSummary.txt"))
#hist(Taxasum$`Proportion Missing`)
#hist(Clustered_data2$ProportionMissing)
#test <- Taxasum[match(tbl$`order$OrigName`,Taxasum$`Taxa Name`),"Proportion Missing"]
#Clustered_data$ProportionMissing <- Taxasum[match(Clustered_data$OrigName,Taxasum$`Taxa Name`),"Proportion Missing"]
#Clustered_data2 <- Clustered_data[which(Clustered_data$`ProportionMissing`<.8),]

for(i in 1:10){
  print (i)
  
  assign(paste("C",i,sep=""),Clustered_data[Clustered_data$clust_names == i,])
}


for(i in 1:10){
  print (i)
  
  assign(paste("K",i,sep=""),as.data.frame(c(get(paste("C",i,sep=""))[sample(nrow(get(paste("C",i,sep=""))), 21), ])))
  
}

for(i in 1:10){

  write.table(c(get(paste("K",i,sep="")))$V1, file = paste("SampleforscatterplotK",i,".txt",sep=""), sep = "\t",
                row.names = FALSE, quote = FALSE)
}

