library(ggplot2)
library(ggmap)
library(RColorBrewer) # for color selection
library(devtools)
#devtools::install_github("cmartin/ggConvexHull")
library(ggConvexHull)

tblv1 <- tbl
tblv1$v100 <- tblv1$V1 * 100
tblv1$v100 <- signif(tblv1$v100 ,2)
tblv1$v100 <- round(tblv1$v100 ,0)
tblv1 <- tblv1[ which(tblv1$v100 > 0),]
tblv1 <- as.data.frame(lapply(tblv1, rep, tblv1$v100))

ggplot(tblv1, aes(x = Longitude, y = Latitude)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", colour="white", data = tblv1) +
  scale_x_continuous(limits = c(-85, -55)) +
  scale_y_continuous(limits = c(-40, 18)) +
  scale_fill_gradient(low = "#9f924400", high = "#9f9244")

tblv2 <- tbl
tblv2$V100 <- tblv2$V2 * 100
tblv2$V100 <- signif(tblv2$V100 ,2)
tblv2$V100 <- round(tblv2$V100 ,0)
tblv2 <- tblv2[ which(tblv2$V100 > 0),]
tblv2 <- as.data.frame(lapply(tblv2, rep, tblv2$V100))

ggplot(tblv2, aes(x = Longitude, y = Latitude)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", colour="white", data = tblv2) +
  scale_x_continuous(limits = c(-85, -55)) +
  scale_y_continuous(limits = c(-40, 18)) +
  scale_fill_gradient(low = "#7f64b900", high = "#7f64b9")

#theme(legend.position='none')

tblv3 <- tbl
tblv3$V100 <- tblv3$V3 * 100
tblv3$V100 <- signif(tblv3$V100 ,2)
tblv3$V100 <- round(tblv3$V100 ,0)
tblv3 <- tblv3[ which(tblv3$V100 > 0),]
tblv3 <- as.data.frame(lapply(tblv3, rep, tblv3$V100)) 

ggplot(tblv3, aes(x = Longitude, y = Latitude)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", colour="white", data = tblv3) +
  scale_x_continuous(limits = c(-85, -55)) +
  scale_y_continuous(limits = c(-40, 18)) +
  scale_fill_gradient(low = "#c3678500", high = "#c36785")
#theme(legend.position='none')

K190 <- tbl[ which(tbl$v1 > 0.9),]
K150 <- tbl[ which(tbl$v1 > 0.5),]
K110 <- tbl[ which(tbl$v1 > 0.1),]
