
subsetGrid <- function(Americas,N,gridSize=.5) {
  lats <- seq(min(Americas$Lat,na.rm = T),max(Americas$Lat,na.rm = T),gridSize)
  longs <- seq(min(Americas$Long,na.rm = T),max(Americas$Long,na.rm = T),gridSize)
  keep <- Americas[0,]
  keepx <- numeric()
  keepy <- numeric()
  keepTaxa <- numeric()
  for (x in 1:(length(lats)-1)) {
    for (y in 1:(length(longs)-1)) {
      cur <- Americas[which(Americas$Lat<lats[x+1] & Americas$Lat>=lats[x] & Americas$Long<longs[y+1] & Americas$Long>=longs[y]),]
      if (nrow(cur)<1) next
      k <- cur[sample(nrow(cur),min(N,nrow(cur)),replace = F),"Taxa"]
      keepx <- c(keepx,rep(x,length(k)))
      keepy <- c(keepy,rep(y,length(k)))
      keepTaxa <- c(keepTaxa,k)
      
    }
  }
  keep <- Americas[match(x = Americas$Taxa,table = keepTaxa),]
  
  return(keep)
}

keep <- subsetGrid(Americas = Americas,N = 2,gridSize = .25)
unique(keep$Country)

length(which(Americas$Population=="Landrace - SEED"))
