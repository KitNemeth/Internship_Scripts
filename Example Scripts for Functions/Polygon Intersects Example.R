#### Rfunction launcher example for extract info from a shape QGS map poligons and assing thecategories to your samples based on coordinates WGS84    
#### Requires dataframe (keyfile), shape file (created in QGIS .shp), id (name of regions in shapefile) and name of lattitude and longitude column in dataframe.
#### Returns a column specified which polygon (region) each idnividual belongs to
source("L:/Krisztian/R Scripts/Functions/Polygon Intersects Function.R")

setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
dataM <- as.data.frame(fread("MasterLandraceTeoInbredGBS_collapseDist0.02_forMDSv3.txt"))
Brazil <- dataM[which(dataM$Country%in%c("BRAZIL","Brazil") & dataM$Lat!=""  & dataM$Long!=""),] 

setwd("L:/Krisztian/QGIS")

df <- PolygonIntersects(Brazil,"Brazil_regions.shp","name1","Lat","Long")
