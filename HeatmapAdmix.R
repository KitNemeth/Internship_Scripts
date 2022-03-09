setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
rownames(tbl) <- tbl$OrigName
tbl[,16] <- NULL
for (i in 1:15) {colnames(tbl)[{i}] <- paste("K",i,sep="") 
} 



tblscaled <- data.matrix(scale(tbl))

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")

library(ComplexHeatmap)
library(circlize)
library(pheatmap)

out="AdmixALwithTeo300heatmap.svg"
svg(out,width=15,height=20)
heat1 <- pheatmap (tblscaled,
                   cluster_rows = TRUE, show_colnames = T,show_rownames = F, fontsize_col=15, cluster_cols = FALSE,cutree_rows = 17)
dev.off()

clust_names <- cutree(heat1$tree_row, k=15) ### cut into 5clusters..decided based on your data visulaization
Clustered_data <- data.frame (tblscaled, clust_names)
head (Clustered_data)
write.table(Clustered_data, file="All_taxa_pheatmap_clusters.txt",
            sep="\t", quote=F)
