library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
source("L:/Krisztian/TRGFunctions_original.R")

taxalist <- read.table(file("AllLandrace_covfilt0.1_taxalist.txt"),skip=1,sep="\t")
taxalist <- as.vector(t(as.matrix(taxalist)))

KinshipPheno <- as.data.frame(fread("AllMaizePheno.txt",sep="\t",stringsAsFactors = F))
KinshipPheno <- data.frame(KinshipPheno[,-1], row.names=KinshipPheno[,1])
KinshipPheno <- KinshipPheno[taxalist,]
KinshipPheno <- cbind(rownames(KinshipPheno), data.frame(KinshipPheno, row.names=NULL))

names(KinshipPheno)[1] <- "<Trait>"

file_name = "Kinship_AllLandrace_Pheno.txt"

write.table(KinshipPheno, file_name, sep="\t", quote = F, row.names = F, col.names = T)
