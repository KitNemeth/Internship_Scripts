library(sf)
library(ggplot2)
library(sp)
library(data.table)
library(writexl)

#Here you set a working directory where files will be saved to/sourced from
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
#Read in your shapefile, many existing shapefiles are available but I made a custom one using ths software QGIS
#https://community.tibco.com/wiki/creating-custom-shapefiles
tt <- read_sf("SA_shapefile.shp")

#Reading in my data with latitude and longitude information
dataM <- as.data.frame(fread("MasterLandraceTeoInbredGBS_collapseDist0.02_forMDSv2Test.txt"))
dataM <- dataM[which(dataM$`Proportion Missing`<.95),]
dataM$Lat = as.numeric(dataM$Lat)
dataM$Long = as.numeric(dataM$Long)
dataM$Elev = as.numeric(dataM$Elev)
andean <- dataM[which(dataM$Lat<=13.45 & dataM$Long>=-90 & dataM$Lat!="" & dataM$Country!="COSTA RICA" & dataM$Country!="TRINIDAD AND TOBAGO" & dataM$Country!="GREDA" & dataM$Country!="SAINT VINCENT AND THE GREDINES" & dataM$Country!="PAMA" & dataM$Country!="NICARAGUA" & dataM$Country!="BARBADOS"),] 

#Creating a new variable of just two columns with the Lat Long data in 
andeanpnts <- andean[ , c("Lat", "Long")] 

#crs=4326 designates the Coordinate Reference Systems (In this case WGS84)
dsf <- sf::st_as_sf(andeanpnts, coords=c("Long","Lat"), crs=4326)
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

#Writes out modified data frame to an excel file 
write_xlsx(andean,"L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810\\andeanregion.xlsx")

plot(map)
