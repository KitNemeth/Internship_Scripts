#### Rfunction launcher example for RGB colour generator based on parameters such as coordinates
#### Does not tolerate missing data must be removed before hand
#### Requires dataframe, and columns specified for Rvar, Gvar and Bvar (however, only one variable is required if desired).
#### Returns a new column with with a unique RGB HEX colour value for each row based on input
source("L:/Krisztian/R Scripts/Functions/RGB Generator Function.R")

setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
dataM <- as.data.frame(fread("MasterLandraceTeoInbredGBS_collapseDist0.02_forMDSv3.txt"))
dataM$Lat = as.numeric(dataM$Lat)
dataM$Long = as.numeric(dataM$Long)
dataM$Elev = as.numeric(dataM$Elev)
andean <- dataM[which(dataM$Lat<=13.45 & dataM$Long>=-90 & dataM$Lat!="" & dataM$Country!="COSTA RICA" & dataM$Country!="TRINIDAD AND TOBAGO" & dataM$Country!="GREDA" & dataM$Country!="SAINT VINCENT AND THE GREDINES" & dataM$Country!="PAMA" & dataM$Country!="NICARAGUA" & dataM$Country!="BARBADOS"),] 

andean$combinedLatLong <- rep("LatLong",nrow(andean))
andean$combinedLatLong_color <- putInRGBSpace(andean,Rvar = "Lat",Gvar= "Long")

andean$combinedGeo <- rep("LLE",nrow(andean))
andean$combinedGeo_color <- putInRGBSpace(andean,Rvar = "Lat",Gvar = "Long",Bvar= "Elev")

