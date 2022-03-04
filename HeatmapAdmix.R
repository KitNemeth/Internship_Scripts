setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
rownames(tbl) <- tbl$OrigName
tbl[,15] <- NULL

tblscaled <- data.matrix(scale(tbl))

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")

library(ComplexHeatmap)
library(circlize)
library(pheatmap)

out="AdmixALwithTeo300heatmap.svg"
svg(out,width=10,height=20)
heat1 <- pheatmap (tblscaled,
                   cluster_rows = TRUE, show_colnames = T, fontsize_row=1, fontsize_col=6, cluster_cols = FALSE,cutree_rows = 15)
dev.off()
clust_names <- cutree(heat1$tree_row, k=15) ### cut into 5clusters..decided based on your data visulaization
Clustered_data <- data.frame (tblscaled, clust_names)
head (Clustered_data)
write.table(Clustered_data, file="All_taxa_pheatmap_clusters.txt",
            sep="\t", quote=F)
