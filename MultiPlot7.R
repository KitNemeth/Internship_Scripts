library(cowplot)
library(gridGraphics)
library("grid")
library("ggplotify")
source("L:/Krisztian/Scripts/TRGFunctionsv3.R")
out="MDSmultiplot.svg"
svg(out,width=22,height=8.5)

line = 1
cex = 2
side = 3
adj=-0.085
par(mfrow=c(1,2))

ggmap(us_main_map) +
  stat_density2d(aes(fill = label, alpha=..level..),
                 geom = "polygon", bins=30,size =0.5, data = Bindtbl, contour=TRUE) +
  scale_fill_manual(values=c("K1"= "#59398d",
                             "K2" = "#6d83da",
                             "K3" = "#cf9d3a",
                             "K4" = "#b54673",
                             "K5" = "#45c097",
                             "K6" = "#ba4c46",
                             "K7" = "#968a3d",
                             "K8" = "#bb6130")) +
  geom_point(data = tbl, size=0.3, aes(x = lon, y = lat,)) +
  theme_classic()   
mtext("a",font = 2, side=side, line=line, cex=cex, adj=adj)

ggmap(us_main_map) +
  stat_density2d(aes(fill = label, alpha=..level..),
                 geom = "polygon", bins=80,size =0.5, data = Bindtbl, contour=TRUE) +
  scale_fill_manual(values=c("K1"="#6778d0",
                             "K2"="#a5a23f",
                             "K3"="#9750a1",
                             "K4"="#51b171",
                             "K5"="#ba496b",
                             "K6"="#b95f37")) +
  geom_point(data = tbl, size=0.3, aes(x = lon, y = lat,)) +
  theme_classic()  
mtext("b",font = 2, side=side, line=line, cex=cex, adj=adj)

dev.off()