library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
dataM <- as.data.frame(fread("taxa_s.lst.Clusterlist"))
dataM$Cluster <- as.factor(dataM$Cluster)
m <- lm(dataM$`Proportion Heterozygous`~dataM$Cluster + dataM$`Proportion Missing`)
summary(m)

hist(dataM$`Proportion Heterozygous`)
library(lmtest)
ct <- coef(m)
library("writexl")
write_xlsx(ct,"coeftestcluster.xlsx")
print(ct)

dataN <- as.data.frame(fread("Segsitesestimate2.txt"))
dataN$Cluster=as.factor(dataN$Cluster)
out="SegEstplot.svg"
svg(out)
ggplot(dataN, aes(x=`Segregating sites`, y=`Estimated effect size`)) +
  geom_point(size=1.8, shape=16, aes(color = Cluster)) +
  scale_color_manual(values = c("1" = "#5d3686", 
                                "2" = "#a8ae3e",
                                "3" = "#6a70d7",
                                "4" = "#69a050",
                                "5" = "#c26abb",
                                "6" = "#45c097",
                                "7" = "#b94a73",
                                "8" = "#6d8dd7",
                                "9" = "#bc7d36",
                                "10" = "#ba4c40"))
dev.off()

out="PiEstplot.svg"
svg(out)
ggplot(dataN, aes(x=`Pi2`, y=`Estimated effect size`)) +
  geom_point(size=1.8, shape=16, aes(color = Cluster)) +
  labs(x = "pi2") +
  scale_color_manual(values = c("1" = "#5d3686", 
                                "2" = "#a8ae3e",
                                "3" = "#6a70d7",
                                "4" = "#69a050",
                                "5" = "#c26abb",
                                "6" = "#45c097",
                                "7" = "#b94a73",
                                "8" = "#6d8dd7",
                                "9" = "#bc7d36",
                                "10" = "#ba4c40"))
dev.off()

plot2
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(readr)
library(dplyr)
library(data.table)
us_bbox2 <- c(left = -120, bottom = -40, right = -30, top = 40)
us_main_map2 <- get_stamenmap(us_bbox2, zoom = 5, maptype = "toner-lite")

dataM <- as.data.frame(fread("Landraceclusterdata.txt"))
colnames(dataM)[4] <- "lon"
colnames(dataM)[3] <- "lat"
dataM$lon=as.numeric(dataM$lon)
dataM$lat=as.numeric(dataM$lat)
dataM$Cluster=as.factor(dataM$Cluster)


out="ClustsampMap3.svg"
svg(out)

ggmap(us_main_map2) + 
  geom_point(data = dataM,size=1.8, shape=16, aes(x = lon, y = lat, color = Cluster)) +
  theme(legend.position="right")  +
  scale_color_manual(values = c("1" = "#5d3686", 
                                "2" = "#a8ae3e",
                                "3" = "#6a70d7",
                                "4" = "#69a050",
                                "5" = "#c26abb",
                                "6" = "#45c097",
                                "7" = "#b94a73",
                                "8" = "#6d8dd7",
                                "9" = "#bc7d36",
                                "10" = "#ba4c40")) 
dev.off()


out="ClustsampHeatmap.svg"
svg(out)

ggmap(us_main_map2) + 
  geom_point(data = dataM,size=1.8, shape=16, aes(x = lon, y = lat, color = `Estimated effect size`)) +
  theme(legend.position="right")

dev.off()

out="PiHeatmap.svg"
svg(out)

ggmap(us_main_map2) + 
  geom_point(data = dataM,size=1.8, shape=16, aes(x = lon, y = lat, color = `PiPerBP`)) +
  theme(legend.position="right")

dev.off()

require(gridExtra)

out="Clustsampcomb.svg"
svg(out)
grid.arrange(plot1, plot2, ncol=2) 
dev.off()


