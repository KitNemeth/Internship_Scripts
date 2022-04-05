if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")
library(tidyverse)
library(ggtree)
library(treeio)
library(ggplot2)
library(data.table)

setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
data <- fread("Source_AL_subsetwithTeo.txt.out", header=F,fill=TRUE)
#colnames(order)[1] <- "label"

n <- data$V1

# transpose all but the first column (name)
data <- as.data.frame(t(data[,-1]))
colnames(data) <- n
data = as.matrix(data)
data <- na.omit(data)
list=as.list(data)


tree <- read.tree("rev_dna_outg.treefile")
x <- as_tibble(tree)
x$label <- gsub("'", "", x$label)
y <- full_join(x, order, by = 'label')
tree <- as.treedata(y)

cls <- list(Heimdallarchaeota = c("Heim_AB125", "Heim_LC2", "Heim_LC3"), 
            Thorarchaeota = c("Baja_Thor", "Thor_AB25", "Thor_MP11T", "Thor_SMTZ45", "Thor_SMTZ_145", "Thor_MP8T", "Thor_MP9T", "Thor_SMTZ_183"), 
            Lokiarchaeota = c("Loki_CR4", "Loki_GC14", "Baja_Loki2", "Baja_Loki1", "Baja_Loki3"),
            Odinarchaeota = "Odin_LCB4")

list=as.list(order)
xy.list <- split(order, seq(nrow(order)))

x <- as.vector(order)

print(order)
tree <- groupOTU(tree, xy.list)
ggtree(tree, layout='ellipse') +
aes(color=Source) +
geom_text2(aes(subset = !isTip, label=label)) +
theme_tree2()

out="Tree.svg"
svg(out,width=10,height=10)

ggtree(tree, aes(color=Source), layout='rectangular') 
geom_text2(aes(subset = !isTip, label=label))

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
