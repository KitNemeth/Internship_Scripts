library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
source("L:/Krisztian/TRGFunctions_original.R")

taxalist <- read.table(file("AllLandrace_covfilt0.1_taxalist.txt"),skip=1,sep="\t")
taxalist <- as.vector(t(as.matrix(taxalist)))
kinship <- as.data.frame(fread("maize_kinship.txt",sep="\t",skip=3,stringsAsFactors = F))
kinship <- data.frame(kinship[,-1], row.names=kinship[,1])
colnames(kinship)<-rownames(kinship)
kinship.matrix_covfilt <- kinship[taxalist2,taxalist2]
