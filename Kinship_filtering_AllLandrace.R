library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
source("L:/Krisztian/TRGFunctions_original.R")

taxalist <- read.table(file("AllLandrace_covfilt0.1_taxalist.txt"),skip=1,sep="\t")
taxalist <- as.vector(t(as.matrix(taxalist)))
kinship <- as.data.frame(fread("maize_kinship.txt",sep="\t",skip=3,stringsAsFactors = F))
kinship <- data.frame(kinship[,-1], row.names=kinship[,1])
colnames(kinship)<-rownames(kinship)
kinship.matrix_covfilt <- kinship[taxalist,taxalist]
colnames(kinship.matrix_covfilt)<-NULL
kinship.matrix_covfilt <- cbind(rownames(kinship.matrix_covfilt), data.frame(kinship.matrix_covfilt, row.names=NULL))

line1 = '##Centered_IBS.SumPk=193806.96778449262'
line2 = '##Matrix_Type=Centered_IBS'
line3 = '13126'
file_name = "maize_Kinship_AllLandrace_R.txt"
cat( line1, line2, line3, file = file_name, sep="\n")
write.table( kinship.matrix_covfilt, file_name, sep="\t", quote = F, row.names = F, col.names = F, append = T )
