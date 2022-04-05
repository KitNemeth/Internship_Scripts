library(sf)
library(ggplot2)
library(sp)
library(data.table)
library(writexl)

#Here you set a working directory where files will be saved to/sourced from
setwd("L:/Krisztian/QGIS")
#Read in your shapefile, many existing shapefiles are available but I made a custom one using ths software QGIS
#https://community.tibco.com/wiki/creating-custom-shapefiles
tt <- read_sf("SA_shapefile.shp")

tt <- read_sf("Mexico_shapefile.shp")

tt <- read_sf("Brazil_regions.shp")

tt <- read_sf("Culturalreg.shp")

#Reading in my data with latitude and longitude information
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
dataM <- as.data.frame(fread("MasterLandraceTeoInbredGBS_collapseDist0.02_forMDSv3.txt"))
dataM <- dataM[which(dataM$`Proportion Missing`<.95),]
dataM$Lat = as.numeric(dataM$Lat)
dataM$Long = as.numeric(dataM$Long)
dataM$Elev = as.numeric(dataM$Elev)
Mexico <- dataM[which(dataM$Country%in%c("MEXICO","C_Mex_Low","NorthMexican","C_Mex_High") & dataM$Lat!=""  & dataM$Long!=""),] 
Brazil <- dataM[which(dataM$Country%in%c("BRAZIL","Brazil") & dataM$Lat!=""  & dataM$Long!=""),] 

#Creating a new variable of just two columns with the Lat Long data in 
andeanpnts <- andean[ , c("Lat", "Long")] 

mexicopnts <- Mexico[ , c("Lat", "Long")] 

Brazilpnts <- Brazil[ , c("Lat", "Long")] 

Americas <- dataM[which(dataM$Population%in%c("Landrace - Ames Stock Center","Landrace - RIMMA","Landrace - NSS","Landrace - AndeanMiguel","Landrace - SEED") & dataM$Lat!=""  & dataM$Elev!=""),]

Americaspnt <- Americas[ , c("Lat", "Long")]
#crs=4326 designates the Coordinate Reference Systems (In this case WGS84)
dsf <- sf::st_as_sf(andeanpnts, coords=c("Long","Lat"), crs=4326)
map <- sf::st_as_sf(tt)

dsf <- sf::st_as_sf(mexicopnts, coords=c("Long","Lat"), crs=4326)
map <- sf::st_as_sf(tt)

dsf <- sf::st_as_sf(Brazilpnts, coords=c("Long","Lat"), crs=4326)
map <- sf::st_as_sf(tt)

dsf <- sf::st_as_sf(Americaspnt, coords=c("Long","Lat"), crs=4326)
map <- sf::st_as_sf(tt)

#Points must be transformed to planar as sf library assumes planar projections
pnts_trans <- st_transform(dsf, 2163)
tt_trans <- st_transform(tt, 2163)

#Joining spatial objects via sf::st_join(), combines the attributes of the polygon objects and points objects
res <- sf::st_join(pnts_trans, tt_trans)

print(res)

#This adds a column to the original data frame containing the name of the spatial polygon a point falls into (NA if it doesnt fall into any polygon)
#Can be modified to other variables such as ID
andean$Region <- res$Name

Mexico$Region <- res$id

Brazil$Region <- res$name1

Americas$Region <- res$name1
#Writes out modified data frame to an excel file 
write_xlsx(Americas,"L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810\\Americasregion.xlsx")

plot(map)