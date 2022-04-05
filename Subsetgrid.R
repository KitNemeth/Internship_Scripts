
subsetGrid <- function(Americas,N,gridSize=.5) {
  lats <- seq(min(Americas$Lat,na.rm = T),max(Americas$Lat,na.rm = T)+gridSize,gridSize)
  longs <- seq(min(Americas$Long,na.rm = T),max(Americas$Long,na.rm = T)+gridSize,gridSize)
  keep <- Americas[0,]
  keepx <- numeric()
  keepy <- numeric()
  keepTaxa <- character()
  for (x in 1:(length(lats)-1)) {
    for (y in 1:(length(longs)-1)) {
      cur <- Americas[which(Americas$Lat<lats[x+1] & Americas$Lat>=lats[x] & Americas$Long<longs[y+1] & Americas$Long>=longs[y]),]
      if (nrow(cur)<1) next
      k <- cur[sample(nrow(cur),min(N,nrow(cur)),replace = F),"Taxa"]
      keepx <- c(keepx,rep(x,length(k)))
      keepy <- c(keepy,rep(y,length(k)))
      keepTaxa <- c(keepTaxa,k)
      #print(keepTaxa)
      print(paste(lats[x],longs[y],paste(k,collapse=","),sep="; "))
    }
  }
  keep <- Americas[match(x = keepTaxa,table = Americas$Taxa),]
  
  return(keep)
}

keep <- subsetGrid(Americas = Americas,N = 1,gridSize = 5)
keep <- keep[which(keep$Taxa!="NA"),]

unique(keep$Country)

length(which(Americas$Population=="Landrace - SEED"))
library("writexl")
write_xlsx(keep,"Americassubset.xlsx")

Teosinte <- dataM[which(dataM$Population%in%("Teosinte")),]
Teosinte <- Teosinte[which(Teosinte$Pedigree!="Z_diploperennis" & Teosinte$Pedigree!="Z_perennis?" & Teosinte$Pedigree!="Z_nicara" & Teosinte$Pedigree!="Z_diplop" & Teosinte$Pedigree!="Z_perenn" & Teosinte$Pedigree!="Z_perennis" & Teosinte$Pedigree!="Z_diploperennis?"),]

Teosinte2 <- Teosinte[sample(nrow(Teosinte), 21), ]
unique(Teosinte2$Accession)
write_xlsx(Teosinte2,"Teosintesubset.xlsx")

