library(data.table)
library("writexl")
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
dataM <- as.data.frame(fread("clusterpropmissing.txt"))
dataM <- dataM[which(dataM$`Proportion missing`<.5),]
summary(dataM$`Proportion missing`)

Taxasum <- as.data.frame(fread("MasterLandraceTeoInbredGBS_collapseDist0.02_collapseSEED_TaxaSummary.txt"))
hist(Taxasum$`Proportion Missing`)
hist(Clustered_data2$ProportionMissing)
test <- Taxasum[match(tbl$`order$OrigName`,Taxasum$`Taxa Name`),"Proportion Missing"]
Clustered_data$ProportionMissing <- Taxasum[match(Clustered_data$OrigName,Taxasum$`Taxa Name`),"Proportion Missing"]
Clustered_data2 <- Clustered_data[which(Clustered_data$`ProportionMissing`<.8),]

hist(tbl$ProportionMissing)
cluster <- list()
for (i in 1:17) {
  cluster[[paste("C",i,sep="")]] <- dataM[dataM$clust_names == paste("C",i,sep=""),]
}
for (i in names(cluster)) {
  print(paste(i,nrow(cluster[[i]][which(cluster[[i]]$`Proportion missing`<.7),]),sep=": "))
} 

clusterpropmiss <- list()
for (i in names(cluster)) {
  clusterpropmiss[[paste("C",i,sep="")]] <- which(cluster[[i]]$`Proportion missing`<.7)
} 


C1 <- Clustered_data2[Clustered_data2$clust_names == '1',]
C2 <- Clustered_data2[Clustered_data2$clust_names == '2',]
C3 <- Clustered_data2[Clustered_data2$clust_names == '3',]
C4 <- Clustered_data2[Clustered_data2$clust_names == '4',]
C5 <- Clustered_data2[Clustered_data2$clust_names == '5',]
C6 <- Clustered_data2[Clustered_data2$clust_names == '6',]
C7 <- Clustered_data2[Clustered_data2$clust_names == '7',]
C8 <- Clustered_data2[Clustered_data2$clust_names == '8',]
C9 <- Clustered_data2[Clustered_data2$clust_names == '9',]
C10 <- Clustered_data2[Clustered_data2$clust_names == '10',]
C11 <- Clustered_data2[Clustered_data2$clust_names == '11',]
C12 <- Clustered_data2[Clustered_data2$clust_names == '12',]
C13 <- Clustered_data2[Clustered_data2$clust_names == '13',]
C14 <- Clustered_data2[Clustered_data2$clust_names == '14',]
C15 <- Clustered_data2[Clustered_data2$clust_names == '15',]
C16 <- Clustered_data2[Clustered_data2$clust_names == '16',]
C17 <- Clustered_data2[Clustered_data2$clust_names == '17',]

df.list <- list("1" = "C1","2" = "C2","3" = "C3","4" = "C4","5" = "C5","6" = "C6","7" = "C7","8" = "C8","9" = "C9","10" = "C10","11" = "C11","12" = "C12","13" = "C13","14" = "C14","15" = "C15","16" = "C16","17" = "C17")

for (i in names(df.list)) {
  print(i)
clus <- df.list[[i]] 
print(clus)
get <- get(clus)
#print(get)

samp <- get[sample(nrow(get), 3), ]
print(samp)


}



C1[sample(nrow(C1), 3), ]

df.list <- list(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,C16,C17)

df.list <- lapply(df.list, FUN = function(i) sample_n(i, 12)) 
data <- do.call("rbind", df.list)

write.table(data, file = "Sampleforscatterplot.txt", sep = "\t",
            row.names = FALSE)
write_xlsx(data,"Sampleforscatterplot.xlsx")
