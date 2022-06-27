install.packages("data.table")

library(data.table)

#' This subsets taxa in a dataframe based on a geographic grid... If you don't have lat lon as named columns in df it will return nULL
#' @param df dataframe with  columns and lat, lon
#' @param n number of taxa from each grid to be kept
#' @param gridsize polygon id, typically "id" or "name"
#' @param latColumn latitude column name, default "lat"
#' @param lonColumn longitude column name, default "lon"
#' @return dataframe with a sampled taxa from specified gridsizes
#' @export
subsetGrid <- function(df,n,gridsize,latColumn="lat", lonColumn="lon"){
  if (((tolower(latColumn)%in%tolower(colnames(df))) | (tolower(lonColumn)%in%tolower(colnames(df))))==FALSE) {
    print("make sure you have columns containing 'lat' and 'lon'")
    return(NULL)
  }
  latCol <- which(tolower(latColumn)==tolower(colnames(df)))
  lonCol <- which(tolower(lonColumn)==tolower(colnames(df)))
  colnames(df)[latCol] <- "lat"
  colnames(df)[lonCol] <- "lon" 
  df$lat <- as.numeric(df$lat)
  df$lon <- as.numeric(df$lon)
  lats <- seq(min(df$lat,na.rm = T), max(df$lat,na.rm = T)+gridsize,gridsize)
  longs <- seq(min(df$lon,na.rm = T), max(df$lon,na.rm = T)+gridsize,gridsize)
  keep <- df[0,]
  keepx <- numeric()
  keepy <- numeric()
  keepTaxa <- character()
  for (x in 1:(length(lats)-1)) {
    for (y in 1:(length(longs)-1)) {
      cur <- df[which(df$lat<lats[x+1] & df$lat>=lats[x] & df$lon<longs[y+1] & df$lon>=longs[y]),]
      if (nrow(cur)<1) next
      k <- cur[sample(nrow(cur),min(n,nrow(cur)),replace = F),"Taxa"]
      keepx <- c(keepx,rep(x,length(k)))
      keepy <- c(keepy,rep(y,length(k)))
      keepTaxa <- c(keepTaxa,k)
      #print(keepTaxa)
      print(paste(lats[x],longs[y],paste(k,collapse=","),sep="; "))
    }
  }
  keep <- df[match(x = keepTaxa,table = df$Taxa),]
  
  return(keep)
}