install.packages("sp")
install.packages("sf")
install.packages("data.table")
install.packages("roxygen2")

library(sp)
library(sf)
library(data.table)
library(roxygen2)

#' This intersects polygon... If you don't have lat lon and elev as named columns in df it will return NULL
#' @param df dataframe with  columns and lat, lon and elev
#' @param shapefile shapefile containing named polygon regions
#' @param id polygon id, typically "id" or "name"
#' @param latColumn latitude column name, default "lat"
#' @param lonColumn longitude column name, default "lon"
#' @return dataframe with a new column called 'Region' that is the intersect...
#' @export
PolygonIntersects <- function(df,shapefile,id,latColumn="lat", lonColumn="lon"){
  if (((tolower(latColumn)%in%tolower(colnames(df))) | (tolower(lonColumn)%in%tolower(colnames(df))))==FALSE) {
    print("make sure you have columns containing 'lat' and 'lon'")
    return(NULL)
  }
  latCol <- which(tolower(latColumn)==tolower(colnames(df)))
  lonCol <- which(tolower(lonColumn)==tolower(colnames(df)))
  colnames(df)[latCol] <- "lat"
  colnames(df)[lonCol] <- "lon"
  dfpnts <- df[ , c(latCol, lonCol)] 
  #####Read in shapefile
  tt <- read_sf(shapefile)
  dsf <- sf::st_as_sf(dfpnts, coords=c("lon","lat"), crs=4326)
  map <- sf::st_as_sf(tt)
  pnts_trans <- st_transform(dsf, 2163)
  tt_trans <- st_transform(tt, 2163)
  res <- sf::st_join(pnts_trans, tt_trans)
  df$Region <- res[,id]
  return(df)
}
