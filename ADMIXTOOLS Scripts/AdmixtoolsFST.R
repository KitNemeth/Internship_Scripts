setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
data = 'eigenstratall'
outfile = 'FSTout.txt'
popfile = 'pops.txt'

#admixtools::run_shiny_admixtools()

library(admixtools)
library(tidyverse)
library(pheatmap)
library(vegan) # for reorder.hclust
library(colorspace) # for hex

pops = NULL
pops = read_lines(popfile)
FSTout = fst(data, pop1 = pops,
             maxmem = 8000,
             maxmiss = 1,
             minmaf = 0,
             maxmaf = 0.5,
             pops2 = NULL,
             outpop = NULL,
             outpop_scale = TRUE,
             transitions = TRUE,
             transversions = TRUE,
             auto_only = TRUE,
             keepsnps = NULL,
             afprod = FALSE,
             fst = TRUE,
             poly_only = c("f2"),
             format = NULL,
             adjust_pseudohaploid = FALSE,
             remove_na = TRUE,
             apply_corr = TRUE,
             qpfstats = FALSE,
             verbose = TRUE,)
# `adjust_pseudohaploid = FALSE` will make sure that you always
# get results, even for populations with only a single
# pseudohaploid sample. Estimates for populations with only a
# single pseudohaploid can be biased, which is why the default
# behavior adjust_pseudohaploid = TRUE can result in an error.

write_tsv(FSTout, outfile)
# convert FST or f2 pairs to square matrix
df=as.data.frame(FSTout[,-4])
df2=rbind(df,setNames(df[,c(2,1,3)],names(df)))
t=xtabs(df2[,3]~df2[,1]+df2[,2])

# weights=t["Hungarian",]-t["Nganasan",]
weights=rowMeans(t) # sort by average distance to other populations

pheatmap (
  1000*t,filename="a.png",
  clustering_callback=function(...)reorder(hclust(as.dist(t)),weights),
  # clustering_callback=function(...)hclust(as.dist(t) ),
  legend=F,border_color=NA,cellwidth=18,cellheight=18,
  treeheight_row=80,treeheight_col=80,
  display_numbers=T,number_format="%.0f",number_color="black",
  colorRampPalette(hex(HSV(c(210,210,130,60,40,20,0) ,c(0,.5,.5,.5,.5,.5,.5),1)))(256)
)
