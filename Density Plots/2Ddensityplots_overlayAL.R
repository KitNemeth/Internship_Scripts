library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(readr)
library(dplyr)
#devtools::install_github("dkahle/ggmap")

tblv1 <- tbl
tblv1$V100 <- tblv1$V1 * 100
tblv1$V100 <- signif(tblv1$V100 ,2)
tblv1$V100 <- round(tblv1$V100 ,0)
tblv1 <- tblv1[ which(tblv1$V100 > 0),]
tblv1 <- as.data.frame(lapply(tblv1, rep, tblv1$V100))

tblv2 <- tbl
tblv2$V100 <- tblv2$V2 * 200
tblv2$V100 <- signif(tblv2$V100 ,2)
tblv2$V100 <- round(tblv2$V100 ,0)
tblv2 <- tblv2[ which(tblv2$V100 > 0),]
tblv2 <- as.data.frame(lapply(tblv2, rep, tblv2$V100))

tblv3 <- tbl
tblv3$V100 <- tblv3$V3 * 300
tblv3$V100 <- signif(tblv3$V100 ,2)
tblv3$V100 <- round(tblv3$V100 ,0)
tblv3 <- tblv3[ which(tblv3$V100 > 0),]
tblv3 <- as.data.frame(lapply(tblv3, rep, tblv3$V100))

tblv4 <- tbl
tblv4$V100 <- tblv4$V4 * 400
tblv4$V100 <- signif(tblv4$V100 ,2)
tblv4$V100 <- round(tblv4$V100 ,0)
tblv4 <- tblv4[ which(tblv4$V100 > 0),]
tblv4 <- as.data.frame(lapply(tblv4, rep, tblv4$V100))

tblv5 <- tbl
tblv5$V100 <- tblv5$V5 * 500
tblv5$V100 <- signif(tblv5$V100 ,2)
tblv5$V100 <- round(tblv5$V100 ,0)
tblv5 <- tblv5[ which(tblv5$V100 > 0),]
tblv5 <- as.data.frame(lapply(tblv5, rep, tblv5$V100))

tblv6 <- tbl
tblv6$V100 <- tblv6$V6 * 600
tblv6$V100 <- signif(tblv6$V100 ,2)
tblv6$V100 <- round(tblv6$V100 ,0)
tblv6 <- tblv6[ which(tblv6$V100 > 0),]
tblv6 <- as.data.frame(lapply(tblv6, rep, tblv6$V100))

tblv7 <- tbl
tblv7$V100 <- tblv7$V7 * 700
tblv7$V100 <- signif(tblv7$V100 ,2)
tblv7$V100 <- round(tblv7$V100 ,0)
tblv7 <- tblv7[ which(tblv7$V100 > 0),]
tblv7 <- as.data.frame(lapply(tblv7, rep, tblv7$V100))

tblv8 <- tbl
tblv8$V100 <- tblv8$V8 * 800
tblv8$V100 <- signif(tblv8$V100 ,2)
tblv8$V100 <- round(tblv8$V100 ,0)
tblv8 <- tblv8[ which(tblv8$V100 > 0),]
tblv8 <- as.data.frame(lapply(tblv8, rep, tblv8$V100))

tblv1$label <- "K1"
tblv2$label <- "K2"
tblv3$label <- "K3"
tblv4$label <- "K4"
tblv5$label <- "K5"
tblv6$label <- "K6"
tblv7$label <- "K7"
tblv8$label <- "K8"


Bindtbl <- rbind(tblv2,tblv3,tblv1,tblv4, tblv5, tblv6, tblv7)
colnames(Bindtbl)[14] <- "lon"
colnames(Bindtbl)[13] <- "lat"

colnames(tbl)[14] <- "lon"
colnames(tbl)[13] <- "lat"
tbl$V100 <- ""
tbl$label <- ""

ggplot(Bindtbl, aes(x = lon, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = label), alpha = 0.4,
                 geom = "polygon", bins=50,size =0.01, data = Bindtbl) +
  scale_x_continuous(limits = c(-85, -40)) +
  scale_y_continuous(limits = c(-60, 18)) +
  geom_point() +
  theme_classic()                 

us_bbox2 <- c(left = -120, bottom = -40, right = -30, top = 40)
us_main_map <- get_stamenmap(us_bbox2, zoom = 5, maptype = "toner-lite")

ggmap(us_main_map)

Bindtbl$lon = as.numeric(Bindtbl$lon)
Bindtbl$lat = as.numeric(Bindtbl$lat)

out="KMapAL.svg"
svg(out)
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
dev.off()


ggmap(us_main_map) +
  stat_density2d(aes(color = label, alpha=..level..),
                 geom = "density2d", bins=50,size =0.5, data = Bindtbl, contour=TRUE) +
  geom_point() + 
theme_classic() 
