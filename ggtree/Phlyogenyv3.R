if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")
library(tidyverse)
library(ggtree)
library(treeio)
library(ggplot2)
library(data.table)

setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
data <- fread("phylosubsetOTUlist.txt.out", header=F,fill=TRUE)
tree <- read.tree("revsubset_dna_outg.treefile")
#x <- as_tibble(tree)
#x$label <- gsub("'", "", x$label)
#y <- full_join(x, order, by = 'label')
#tree <- as.treedata(y)
n <- data$V1
data <- as.data.frame(t(data[,-1]))
colnames(data) <- n
list=as.list(data)
#xy.list <- split(order, seq(nrow(order)))

#x <- as.vector(order)

#print(order)
tree <- groupOTU(tree, list)
#ggtree(tree, layout='ellipse') +
#aes(color=Source) +
#geom_text2(aes(subset = !isTip, label=label)) +
#theme_tree2()

out="Tree.svg"
svg(out,width=10,height=10)

ggtree(tree, aes(color=group), layout='rectangular')
#geom_text2(aes(subset=!isTip, label=node), hjust=-.3) + #geom_label2(aes(label=label, subset = !is.na(as.numeric(label)) & as.numeric(label) < 60)) +
 scale_color_manual(values=c("#bb486a",
                            "#45c097",
                            "#583687",
                            "#aeae3f",
                            "#6a70d7",
                            "#61a654",
                            "#c074cc",
                            "#9e8038",
                            "#628ed6",
                            "#cb6d34",
                            "#b5508f",
                            "#b74943"))
dev.off()
tree <- as.phylo(tree)
write.jtree(tree,"ggtree_tree.nw")

ape::write.tree(tree, file='tree.txt') 

out="Tree.svg"
svg(out,width=10,height=20)
ggtree(tree, size = 1,aes(color=Source)) + theme_tree2()
geom_cladelab(node=1411, label="Teosinte", align=TRUE, textcolor='black', barcolor='black', angle=90, offset.text=0.01, hjust=0.5, fontsize=3) 

dev.off()
