library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(readr)
library(dplyr)
devtools::install_github("dkahle/ggmap")

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

tblv1$label <- "K1"
tblv2$label <- "K2"
tblv3$label <- "K3"
tblv4$label <- "K4"
tblv5$label <- "K5"
tblv6$label <- "K6"

Bindtbl <- rbind(tblv2,tblv3,tblv1,tblv4, tblv5, tblv6)
colnames(Bindtbl)[12] <- "lon"
colnames(Bindtbl)[11] <- "lat"

colnames(tbl)[12] <- "lon"
colnames(tbl)[11] <- "lat"
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

us_bbox <- c(left = -85, bottom = -57, right = -30, top = 18)
us_main_map <- get_stamenmap(us_bbox, zoom = 5, maptype = "terrain")

us_bbox <- c(left = -85, bottom = -57, right = -30, top = 18)
us_main_map <- get_stamenmap(us_bbox, zoom = 5, maptype = "toner-lite")

ggmap(us_main_map)

out="KMap.svg"
svg(out)

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
dev.off()


ggmap(us_main_map) +
  stat_density2d(aes(color = label, alpha=..level..),
                 geom = "density2d", bins=50,size =0.5, data = Bindtbl, contour=TRUE) +
  geom_point() + 
theme_classic() 
