library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(readr)
library(dplyr)

#install.packages("data.table")
library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
source("L:/Krisztian/TRGFunctions.R")
#dataM <- read.table("MasterLandraceTeoInbredGBS_collapseDist0.02_forMDS.txt",header=T,as.is = T,sep = "\t")
dataM <- as.data.frame(fread("MasterLandraceTeoInbredGBS_collapseDist0.02_ForMDSv3.txt"))
str(dataM)
CIMMYT_12S_13S_RIMMA_Teosinte <- dataM[which(dataM$Taxa%in%taxalist),]
CIMMYT_12S_13S_RIMMA_Teosinte <- CIMMYT_12S_13S_RIMMA_Teosinte[which(CIMMYT_12S_13S_RIMMA_Teosinte$Lat!="vajo" & CIMMYT_12S_13S_RIMMA_Teosinte$Lat!="Sonora" & CIMMYT_12S_13S_RIMMA_Teosinte$Lat!="Siloa" & CIMMYT_12S_13S_RIMMA_Teosinte$Lat!="NM" & CIMMYT_12S_13S_RIMMA_Teosinte$Lat!="ColoradoRiver" & CIMMYT_12S_13S_RIMMA_Teosinte$Lat!="chihuahua" & CIMMYT_12S_13S_RIMMA_Teosinte$Lat!=""),]

dataM <- as.data.frame(fread("TaxaOrder_AL_subsetwithTeo.txt"))

Landracemap <- dataM[which(dataM$Source%in%c("Swarts et al., 2017","Romero Navarro et al., 2017")),] 
#out="CIMMYT_12S_13S_RIMMA_Teosinte.svg"
#svg(out)
MapFromDF(wgs = CIMMYT_12S_13S_RIMMA_Teosinte,yCol = "Lat",xCol = "Long",labelCol = "Pedigree",plotname = "CIMMYT 12S 13S RIMMA_Teosinte",elevation = T,labels = F,ptCol = "darkorchid1",colorCol="Geographic_color",resolutionMeters=100,ptSize = 1,pdfFile = "CIMMYT_12S_13S_RIMMA_Teosinte.svg")
#dev.off()

taxalist <- read.table(file("AllLandrace_covfilt0.1_taxalist.txt"),skip=1,sep="\t")
taxalist <- as.vector(t(as.matrix(taxalist)))

us_bbox2 <- c(left = -120, bottom = -40, right = -30, top = 40)
us_main_map2 <- get_stamenmap(us_bbox2, zoom = 5, maptype = "toner-lite")

colnames(dataM)[6] <- "lon"
colnames(dataM)[5] <- "lat"
dataM$lon=as.numeric(dataM$lon)
dataM$lat=as.numeric(dataM$lat)

out="LandracesMap.svg"
svg(out)

map1 <-ggmap(us_main_map2) +
  geom_point(data = Landracemap,size=0.8, stat = "unique", shape=16, alpha=0.5, aes(x = lon, y = lat, colour =Source)) + 
  guides(colour = guide_legend(override.aes = list(size=3, alpha = 1)), fill = "none") +
  theme(legend.position="bottom") +
  theme(legend.key = element_rect(fill = "white"))

dev.off()

Andeanmap <- dataM[which(dataM$Population%in%("Landrace - AndeanMiguel")),] 
colnames(Andeanmap)[14] <- "lon"
colnames(Andeanmap)[13] <- "lat"
Andeanmap$lon=as.numeric(Andeanmap$lon)
Andeanmap$lat=as.numeric(Andeanmap$lat)
colnames(Andeanmap)[15] <- "Elev"

us_bbox3 <- c(left = -95, bottom = -40, right = -30, top = 20)
us_main_map3 <- get_stamenmap(us_bbox3, zoom = 5, maptype = "toner-lite")

out="AndeanMap.svg"
svg(out)
map2 <- ggmap(us_main_map3) +
  geom_point(data = Andeanmap,size=1.8, stat = "unique", shape=16, aes(x = lon, y = lat, colour =Elev)) + 
  theme(legend.position="bottom") +
  scale_colour_gradient(low = "red", high = "blue") +
labs(col="Elevation (m)")

dev.off()

library(cowplot)
library(svglite)

out="combineMap.svg"
svg(out)
plot_grid(map1, map2, labels = "auto", align = "h")

ggsave("combmap.svg", width = 8, height = 5, scale = 1.2)

dev.off()

colnames(keep)[14] <- "lon"
colnames(keep)[13] <- "lat"
keep$lon=as.numeric(keep$lon)
keep$lat=as.numeric(keep$lat)

"#6582ca",
"#60b348",
"#a066c6",
"#b6b23e",
"#d34997",
"#4fa371",
"#d14c3f",
"#4bbfce",
"#df9446",
"#c1647b",
"#7d833b",
"#a56638"

out="AdmixClusterMap.svg"
svg(out)

ggmap(us_main_map2) + 
  geom_point(data = dataM,size=1.8, shape=16, aes(x = lon, y = lat, colour = Cluster)) +
  theme(legend.position="bottom") +
  scale_color_manual(values = c("Cluster 1" = "#6582ca",
                                "Cluster 2" = "#60b348",
                                "Cluster 3" = "#a066c6",
                                "Cluster 4" = "#b6b23e",
                                "Cluster 5" = "#d34997",
                                "Cluster 6" = "#4fa371",
                                "Cluster 7" = "#d14c3f",
                                "Cluster 8" = "#4bbfce",
                                "Cluster 9" = "#df9446",
                                "Cluster 10" = "#c1647b",
                                "Cluster 11" = "#7d833b",
                                "Cluster 12" = "#a56638")) 
dev.off()

ggmap(us_main_map2) + 
  geom_point(data = dataM,size=1.8, shape=16, aes(x = lon, y = lat, colour = Beck)) +
  theme(legend.position="bottom")
