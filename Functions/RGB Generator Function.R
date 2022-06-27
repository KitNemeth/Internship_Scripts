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
  return(rgb(r,g,b,maxColorValue = 255))
}
