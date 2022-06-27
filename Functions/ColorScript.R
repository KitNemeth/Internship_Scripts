install.packages("data.table")

library(data.table)

#' Genereates R G B values for variables in a dataframe... If you don't have lat lon and elev as named columns in df it will return NULL
#' @param df dataframe with  columns and lat, lon and elev
#' @param Rvar column name that will be red
#' @param Gvar column name that will be green
#' @param Bvar column name that will be blue
#' @return dataframe with a new column called 'Region' that is the intersece...
#' @export

putInRGBSpace <- function(df,Rvar=NULL,Gvar=NULL,Bvar=NULL) {
  
  df[,Rvar] <- as.numeric(df[,Rvar])
  df[,Gvar] <- as.numeric(df[,Gvar])
  df[,Bvar] <- as.numeric(df[,Bvar])
  
  keep <- which(is.na(df[,Rvar])==F & is.na(df[,Gvar])==F & is.na(df[,Bvar])==F)
  out <- rep(NA,nrow(df))
  
  if (is.null(Rvar)==F) df <- df[which(is.na(df[,Rvar])==F),]
  if (is.null(Gvar)==F) df <- df[which(is.na(df[,Gvar])==F),]
  if (is.null(Bvar)==F) df <- df[which(is.na(df[,Bvar])==F),]
  if (nrow(df)==0) {
    print("Nothing left after filtering for NAs!")
    return(NULL)
  }
  if (is.null(Rvar)) {r <- rep(0,nrow(df))
  }else {r <- ((df[,Rvar]-min(df[,Rvar]))/(max(df[,Rvar])-min(df[,Rvar])))*255}
  if (is.null(Gvar)) {g <- rep(0,nrow(df))
  }else {g <- ((df[,Gvar]-min(df[,Gvar]))/(max(df[,Gvar])-min(df[,Gvar])))*255}
  if (is.null(Bvar)) {b <- rep(0,nrow(df))
  }else {b <- ((df[,Bvar]-min(df[,Bvar]))/(max(df[,Bvar])-min(df[,Bvar])))*255}
  print(rgb(r,g,b,maxColorValue = 255))

    out[keep] <- rgb(r,g,b,maxColorValue = 255)
  return(out)
}

#example that produces plot to show colour scale produced
pick <- sample(1:nrow(andean),10,F)
alt <- andean[pick,"Elev"]
lat <- andean[pick,"Lat"]
long <- andean[pick,"Long"]
elev <- data.frame(alt,lat,long)
elev$combinedGeo <- rep("altlatlong",nrow(elev))
elev$combinedGeo_color <- putInRGBSpace(elev,Rvar = "lat",Gvar = "long", Bvar = "alt")
plot(elev$long,elev$lat,col=elev$combinedGeo_color,pch=19,cex=3)


####feed dataframe with Lat, Long and Elev columns
####Creates column with string replicated on every lines
andean$combinedGeo <- rep("LLE",nrow(andean))
####putInRGBSPACE function will create an RGB value for each specified column (e.g Red for Lat, Green for Long and Blue) for elev), 
andean$combinedGeo_color <- putInRGBSpace(andean,Rvar = "Lat",Gvar = "Long",Bvar= "Elev")
plot(andean$Lat,andean$Long,col=andean$combinedGeo_color,pch=19,cex=3)

dataM$combinedGeo <- rep("LLE",nrow(dataM))
dataM$combinedGeo_color <- putInRGBSpace(dataM,Rvar = "Lat",Gvar = "Long",Bvar= "Elev")

andean$combinedLatLong <- rep("LatLong",nrow(andean))
andean$combinedLatLong_color <- putInRGBSpace(andean,Rvar = "Lat",Gvar= "Long")
plot(andean$Lat,andean$Long,col=andean$combinedLatLong_color,pch=19,cex=3)

Americas$combinedGeo <- rep("LLE",nrow(Americas))
Americas$combinedGeo_color <- putInRGBSpace(Americas,Rvar = "Lat",Gvar = "Long",Bvar= "Elev")
plot(Americas$Lat,Americas$Long,col=Americas$combinedGeo_color,pch=19,cex=3)

Americas$combinedLatLong <- rep("LatLong",nrow(Americas))
Americas$combinedLatLong_color <- putInRGBSpace(Americas,Rvar = "Lat",Gvar= "Long")
plot(Americas$Lat,Americas$Long,col=Americas$combinedLatLong_color,pch=19,cex=3)

MexSA$combinedGeo <- rep("LatLongElev",nrow(MexSA))
MexSA$combinedGeo_color <- putInRGBSpace(MexSA,Rvar = "Lat",Gvar = "Long",Bvar= "Elev")
plot(MexSA$Lat,MexSA$Long,col=MexSA$combinedGeo_color,pch=19,cex=3)

dataM$Lat <- as.numeric(dataM$Lat)
dataM$Long <- as.numeric(dataM$Long)
dataM$Elev <- as.numeric(dataM$Elev)

dataM$combinedLatLong <- rep("LatLong",nrow(dataM))
dataM$combinedLatLong_color <- putInRGBSpace(dataM,Rvar = "Lat",Gvar= "Long")
plot(MexSA$Lat,MexSA$Long,col=MexSA$combinedLatLong_color,pch=19,cex=3)
