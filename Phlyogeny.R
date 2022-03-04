if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")
library(tidyverse)
library(ggtree)
library(treeio)
library(ggplot2)

setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
order <- fread("Source_AL_subsetwithTeo.txt", header=T)
colnames(order)[1] <- "label"

tree <- read.tree("Phylogeny.nwk")
x <- as_tibble(tree)
x$label <- gsub("'", "", x$label)
y <- full_join(x, order, by = 'label')
tree <- as.treedata(y)

out="Tree.svg"
svg(out,width=20,height=10)
ggtree(tree, aes(color=Source), layout='Cladogram')
dev.off()

out="Tree.svg"
svg(out,width=10,height=20)
ggtree(tree, size = 1,aes(color=Source)) + theme_tree2() +
geom_cladelab(node=1411, label="Teosinte", align=TRUE, textcolor='black', barcolor='black', angle=90, offset.text=0.01, hjust=0.5, fontsize=3) 

dev.off()


ggtree(beast_tree, aes(color=rate)) +
  scale_color_continuous(low='darkgreen', hig h='red') +
  theme(legend.position="right")

ggtree(tree, size = 1) + theme_tree2() +  geom_tiplab()
