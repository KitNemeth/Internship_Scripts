install.packages("data.table")

library(data.table)

#' Genereates dataframe with ancestry rows multiplied by their value for use in density plot
#' @param file prefix to admixture results (tbl dataframe  with  columns admixture prpoportion columns sourced from this prefix)
#' @param order dataframe with accession information (name, lat, lon etc...)
#' @param k number of Ks (columns for ancestry)
#' @param latColumn latitude column name, default "lat"
#' @param lonColumn longitude column name, default "lon"
#' @return dataframe rows multiplied by their ancestry value for a given K...
#' @export

ancestrydensity <- function(file, order, k, latColumn="lat", lonColumn="lon") {
  tbl=read.table(paste(file,".",k,".Q",sep = ""))
  match <- match(x = read.table(paste(file,"fam",sep="."),header=F,as.is=T)[,2],table = order$OrigName)
  order <- order[match,]
  tbl <- cbind(tbl,order)
  tbl <- cbind(tbl,order$OrigName)
  tbl <- tbl[order(tbl$OrigOrder),]

  ks <- tbl[0,]
  ks$V100 <- rep(paste(""),0)
  ks$label <- rep(paste(""),0)
  
  for (i in 1:k){
    curk <- tbl
    vcol <- paste("V",i,sep="")
    curk$V100 <- curk[,vcol] * 100
    curk$V100 <- signif(curk$V100 ,2)
    curk$V100 <- round(curk$V100 ,0)
    curk <- curk[ which(curk$V100 > 0),]
    curk <- as.data.frame(lapply(curk, rep, curk$V100))
    curk$label <- rep(paste("K", i, sep = ""),nrow(curk))
    ks <- rbind(ks,curk)
 
  }
  latCol <- which(tolower(latColumn)==tolower(colnames(ks)))
  lonCol <- which(tolower(lonColumn)==tolower(colnames(ks)))
  colnames(ks)[latCol] <- "lat"
  colnames(ks)[lonCol] <- "lon" 
  return(ks)
}

