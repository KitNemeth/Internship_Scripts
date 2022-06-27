setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810/pops")

library(data.table)
library(admixtools)
library(tidyverse)
library(pheatmap)
library(vegan) # for reorder.hclust
library(colorspace) # for hex

WCFST <- fread("WCFST.out")

df=as.data.frame(WCFST[,-4])
df2=rbind(df,setNames(df[,c(2,1,3)],names(df)))
t=xtabs(df2[,3]~df2[,1]+df2[,2])

# weights=t["Hungarian",]-t["Nganasan",]
weights=rowMeans(t) # sort by average distance to other populations

pheatmap (
  1000*t,filename="WCFST.png",
  clustering_callback=function(...)reorder(hclust(as.dist(t)),weights),
  # clustering_callback=function(...)hclust(as.dist(t) ),
  legend=F,border_color=NA,cellwidth=18,cellheight=18,
  treeheight_row=80,treeheight_col=80,
  display_numbers=T,number_format="%.0f",number_color="black",
  colorRampPalette(hex(HSV(c(210,210,130,60,40,20,0) ,c(0,.5,.5,.5,.5,.5,.5),1)))(256)
)
