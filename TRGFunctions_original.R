#####These are functions for interacting with data from the Swarts Lab, especially designed to connect to the lab azure database (trg-srv.postgres.database.azure.com)
###Kelly Swarts, 2019

Sys.setenv(lang="EN")
library(Hmisc)
library(RColorBrewer)
library(bio3d)
library(jsonlite)
library(R.utils)
library(stringr)
library(rworldmap)
library(elevatr)
library(dplR)
library(raster)
library(rgdal)

options(digits=6)

###Global variables for mapping 
col.status <- c("#000000","#2F4F4F","#778899","#A9A9A9","#DCDCDC")
names(col.status) <- c("Live","Standing Dead","Fallen Dead","Uprooted","Stump")
col.species <- c("#5ea45e","#c09b1f","#F1E2CC","#A65628","#FF7F00","#E7298A","#FDDAEC","#BEBADA","#7FC97F","#FDBF6F","#CCCCCC","#FFFFB3","#377EB8","#DECBE4","#E5D8BD","#FFFF99","#666666","#D9D9D9","#B15928","#FFF2AE","#FFFFCC","#CCEBC5","#66A61E","#B3DE69","#B3E2CD","#7570B3","#E31A1C","#33A02C","#E41A1C","#B2DF8A","#F0027F","#1F78B4","#FFFF99","#BC80BD","#CBD5E8","#F781BF","#FB8072","#FC8D62","#FFD92F","#984EA3","#FFFF33","#80B1D3","#FDC086","#B3CDE3","#6A3D9A","#FFED6F","#BEAED4","#F4CAE4","#999999","#D95F02","#386CB0","#E6AB02","#E78AC3","#8DD3C7","#E6F5C9","#FDB462","#A6D854","#E5C494","#A6761D","#CAB2D6","#FF7F00","#A6CEE3","#CCEBC5","#FB9A99","#66C2A5","#FED9A6","#FCCDE5","#FBB4AE","#F2F2F2","#666666","#B3B3B3","#4DAF4A","#BF5B17")
names(col.species) <- seq(1,33,1)
names.species <- c("Picea abies","Pinus sylvestris","Fagus sylvatica","Carpinus betulus","Fraxinus spp.","Ilex aquifolium","Larix spp.","Populus spp.","Betula spp.","Quercus spp.","Pseudotsuga menziesii","Abies spp.","Juniperus spp.","Cupressus spp.","Arabidopsis thaliana","Zea mays mays","Zea mays mexicana","Zea mays parviglumis","Acer psudoplatanus","Sorbus acuparia","Petasites spp","Athyrium filix-femina","Abies alba","Acer spp.","UNK spp.","Ips typographus","Abies concolor","Pinus ponderosa","Gossypium spp.","Cedrus spp.","Cannabis sativa","Salix spp.","Pinus mugo")


####Columns for different db tables
plotCols <- c("plotid","collected","slope","aspect","altitude","centerpoint","ne","se","nw","sw","country","recorder","person_soilmoisture","person_dna","photolocation","comment","altname")

##Connect by odbc
#' Get a connection through a DSN to a database. Check out the following for generating a functional DSN on mac: https://www.boriel.com/postgresql-odbc-connection-from-mac-os-x.html
#' @param DSN the DSN connection name. The default is "azure-trg" is none supplied. This will obviously only work if there is a DSN called azure-trg set up.
#' @return A functional connection or NULL if connection does not work
#' @export
getCon <- function(DSN) {
  con <- tryCatch({
    DBI::dbConnect(odbc::odbc(), DSN)
  }, error = function(err) {
    message(paste("Problem with connection. Please check DSN:",DSN))
    return(NULL)
  })
  return(con)
}

##Query trgdb
#' Return a query using a DSN connection string for the database of choice
#' @param DSN the DSN connection name. The default is "azure-trg" is none supplied. This will obviously only work if there is a DSN called azure-trg set up.
#' @param query A appropriate *sql language query for the database. Includes the ';' at the end automatically
#' @return A result (via dbFetch()) or NULL if connection does not work
#' @export
getDataFromDB <- function(DSN="azure-trg",query) {
  con <- getCon(DSN)
  if (is.null(con)) return(NULL)
  res <- tryCatch({
    dbSendQuery(conn = con, statement = paste(query,";",sep=""))
  }, error = function(err) {
    message(paste("Problem with query:",query))
    return(NULL)
  })
  if (is.null(res)) return(res)
  mo <- dbFetch(res)
  dbClearResult(res)
  dbDisconnect(con)
  return(mo)
}

makePlateMap <- function(plateids,table="dnaextraction",pdfFile="./plateMap.pdf") {
  sam <- getDataFromDB(query = paste("Select * from",table,"where plateid in (",paste(plateids,collapse = ","),") order by plateid,position;"))
  sam$altname <- str_replace(sam$altname,pattern = "BLANK",replacement = "BL")
  pdf(pdfFile,width = (4.7*2),height = (3.15*2))
  par(mar=c(0,0,0,0.2),oma=c(0,0,0,0),mfrow=c(2,2))
  for (plate in plateids) {
    types=unique(sam[which(sam$plateid==plate),"tissue"])
    types <- types[order(types)]
    types <- types[-which(types=="BLANK")]
    plot <- paste(unique(sam$plotid[which(sam$plateid==plate)]),collapse = ",")
    for (cur in c("","BACKUP")) {
      plot(NULL,yaxt='n',xaxt='n',xlim=c(0,12.5),ylim=c(0,9.2),bty="n",xaxs="i",yaxs="i")
      rect(xleft = c(.5,2.5,4.5,6.5,8.5,10.5),ybottom = 0,xright = c(1.5,3.5,5.5,7.5,9.5,11.5),ytop = 8,col = "lightgrey")
      abline(h=c(0,1,2,3,4,5,6,7,8,8.5),v = c(0,.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5))
      rect(xleft = 0,ybottom = c(0,8,8),xright = c(.5,12.5,12.5),ytop = c(9.2,9.2,8.5),col = "white")
      title(main = paste(" PlateID ",plate," - Plot ",plot," (",paste(types,collapse=","),") ",cur,sep=""),line =-1.1,adj = 0,cex.main=.85)
      text(x=.4,y=c(7.5,6.5,5.5,4.5,3.5,2.5,1.5,.5),labels=c("A","B","C","D","E","F","G","H"),adj=1,font=2,cex=1.2)
      text(x=seq(1,12),y=8.25,labels=seq(1,12),font=2,cex=1.2)
      for (col in 1:12) {
        for (row in 1:8) {
          if (length(sam[which(sam$plateid==plate & sam$position==(((col-1)*8)+row)),"altname"])>0) {
            text(x=col,y=9-row-.5,labels=sam[which(sam$plateid==plate & sam$position==(((col-1)*8)+row)),"altname"],font=2,cex=.75)
            print(paste(col,(9-row-.5),(((col-1)*8)+row),sam[which(sam$plateid==plate & sam$position==(((col-1)*8)+row)),"altname"]))
          }
        }
      }
    }
    if (which(plateids==plate)%%2==0) {
      for (i in 1:4) {plot(NULL,yaxt='n',xaxt='n',xlim=c(0,12.5),ylim=c(0,9.2),bty="n",xaxs="i",yaxs="i")}
    }
  }
  dev.off()
}
#makePlateMap(plateids = c(138,139),pdfFile = "/Users/kelly.swarts/Dropbox (VBC)/FieldData/PlateLabels/MultiplePlates_138_139")

###Functions to process tree-ring files
splitNameTRG <- function(string,repRows=1) {
  name <- str_pad(strsplit(string,"_")[[1]][1], 9, pad = "0")
  appendName <- strsplit(string,"_")[[1]][2]
  sampleName <- name
  #if (is.na(appendName)==F) sampleName <- paste(name,appendName,sep = "_")
  name <- strsplit(name, "")[[1]]
  return(as.data.frame(cbind(sample=rep(sampleName,repRows),plot=rep(paste(name[3],name[4],name[5],sep = ""),repRows),tree=rep(paste(name[7],name[8],sep = ""),repRows),core=rep(name[9],repRows))))
}

readTucsonPhen <- function(rwl,header=F) {
  df <- read.tucson(rwl,"tucson",header=header)
  osms <- list()
  osm <- data.frame(c(sample=character(0),plot=character(0),tree=character(0),core=character(0),width=numeric(0)),stringsAsFactors = F)
  for (core in 1:ncol(df)) {
    curr <- as.matrix(df[,core])
    names(curr) <- as.numeric(rownames(df))
    curr <- curr[which(is.na(curr)==F)]
    name <- str_pad(strsplit(colnames(df)[core],"_")[[1]][1], 9, pad = "0")
    currOSM <- data.frame(splitNameTRG(colnames(df)[core],length(curr)),date=as.numeric(names(curr)),width=as.numeric(curr),stringsAsFactors=F)
    if (name%in%names(osms)) {
      osms[[name]] <- rbind(osms[[name]],currOSM)
      allD <- seq(min(osms[[name]]$date),max(osms[[name]]$date),1)
      missing <- allD[which(allD%in%osms[[name]]$date==F)]
      print(paste("Core",name,"is missing years",paste(missing,collapse = ", ")))
      osms[[name]] <- rbind(osms[[name]],data.frame(splitNameTRG(name,length(missing)),date=missing,width=rep(NA,length(missing)),stringsAsFactors=F))
      osms[[name]] <- osms[[name]][order(osms[[name]]$date,decreasing = F),]
    } else {osms[[name]] <- currOSM}
  }
  finalOsm <- data.frame(c(sample=character(0),plot=character(0),tree=character(0),core=character(0),date=numeric(0),width=numeric(0),AR1=numeric(0),AR2=numeric(0),AR3=numeric(0),AR4=numeric(0),AR5=numeric(0),AR6=numeric(0),AR7=numeric(0),AR8=numeric(0),AR9=numeric(0),AR10=numeric(0)),stringsAsFactors = F)
  for (curr in osms) {
    for (ar in 1:10) {
      curr[,paste("AR",ar,sep="")]=c(rep(NA,ar),curr$width)[1:nrow(curr)]
    }
    finalOsm <- rbind(finalOsm,curr)
  }
  return(finalOsm)
}

rwlsToOSM <- function(rwlFiles=params$rwls) {
  rwls <- strsplit(rwlFiles,",")[[1]]
  osm <- readTucsonPhen(rwls[1])
  if (length(rwls)>1) {
    for (curr in 2:length(rwls)) {
      add <- readTucsonPhen(rwls[curr])
      add <- add[which(add$sample%in%osm$sample==F),]
      osm <- rbind(osm,add)
    }
  }
  i <- sapply(osm, is.factor)
  osm[i] <- lapply(osm[i], as.character)
  return(osm)
}

addPithToOSM <- function(osm,pithInput=params$pith) {
  if (is.null(pithInput)) {
    print("No pith values to add")
    return(osm)
  }
  print("Adding estimated pith values")
  pfiles <- strsplit(pithInput,",")[[1]]
  piths <- read.csv(pfiles[1],sep = ",",header=T,as.is = T)
  if (length(pfiles)>1) {
    for (curr in 2:length(piths)) {
      piths <- rbind(piths,read.csv(pfiles[curr],sep = ",",header=T,as.is = T))
    }
  }
  if (nrow(piths)<1) {
    print("No pith values to add")
    return(osm)
  }
  piths$core <- str_pad(unlist(lapply(strsplit(piths$X...Sample,"_"), `[[`, 1)), 9, pad = "0")
  uni <- unique(piths[,c("core","EstimatedPith")])
  osm$FirstYear <- uni[match(x = osm$sample,uni$core,nomatch = NA),"EstimatedPith"]+1
  return(osm)
}

addDetrendToOSM <- function(osm) {
  ####Could try by taking neg exp from pith date. If NA, make linear decay
  osm$negExp <- numeric(nrow(osm))
  osm$linear <- numeric(nrow(osm))
  osm$widthLog <- osm$width
  osm$widthLog[which(osm$widthLog==0)] <- .Machine$double.xmin
  osm$widthLog <- log(osm$widthLog)
  osm$FirstYearImp <- numeric(nrow(osm))
  osm$AgeAtYear <- numeric(nrow(osm))
  pithDate <- F
  if ("FirstYear"%in%names(osm)) {
    osm$pithDetrend <- numeric(nrow(osm))
    pithDate <- T
  }
  for (samp in levels(factor(osm$sample))) {
    curr <- which(osm$sample%in%samp)
    if (pithDate) {
      FirstYearDate <- unique(osm$FirstYear[curr])
      if (is.na(FirstYearDate)==F & min(osm$date[curr])<FirstYearDate) {
        printf("\n%s has min date less than estimated pith! FIX",samp)
        return(osm)
      }
      if (is.na(FirstYearDate)) {
        tree <- which(osm$tree%in%osm$tree[which(osm$sample%in%samp)])
        impDate <- unique(osm$FirstYearImp[tree])
        if (length(impDate)>1) {
          FirstYearDate <- impDate[which(impDate>0)]
          printf("\nAlready estimated first year for %s as %s",samp,FirstYearDate)
        } else if ("dbhcm"%in%names(osm) & is.na(osm$dbhcm[curr][1])==F) {
          core <- aggregate(osm$date[tree],list(osm$sample[tree]),min)
          core <- core[which(core[,2]==min(core[,2])),1]
          est <- which(osm$sample%in%core & is.na(osm$width)==F)
          ##Based on neg exp decay*mean(longest core)
          decay <- mean(osm$width[est])*exp(1/1:1000)
          missing <- (mean(osm$dbhcm[tree]/2)*10)-(sum(osm$width[est]))
          for (i in 1:length(decay)) {
            if (sum(decay[1:i]) > missing) {
              FirstYearDate <- min(osm$date[est])-i
              break
            }
          }
          printf("\nEstimated pith date for %s based on DBH=%s from early date %s as %s",samp,mean(osm$dbhcm[est]),min(osm$date[est]),FirstYearDate)
        } else {
          FirstYearDate <- min(osm$date[tree])-round(length(unique(osm$date[tree]))/2)
          printf("\nEstimated first year for %s as .5X rings: %s (Earliest observed is %s)",samp,FirstYearDate,min(osm$date[curr]))
        }
      }
      osm$FirstYearImp[curr] <- FirstYearDate
      osm$AgeAtYear[curr] <- osm$date[curr]-FirstYearDate
    }
    #purely neg exp decay based on observed dates
    decay <- exp(1/(1:(max(osm$date[curr])-min(osm$date[curr])+1)))
    decay <- (decay/mean(decay))-1
    names(decay) <- seq(min(osm$date[curr]),max(osm$date[curr]),1)
    osm$negExp[curr] <- decay[match(osm$date[curr],names(decay))]
    #purely linear decay based on observed dates
    decayLin <- ((max(osm$date[curr])-min(osm$date[curr])+1):1)/(max(osm$date[curr])-min(osm$date[curr])+1)
    decayLin <- (decayLin/mean(decayLin))-1
    names(decayLin) <- seq(min(osm$date[curr]),max(osm$date[curr]),1)
    osm[curr,"linear"] <- decayLin[match(osm$date[curr],names(decayLin))]
    if (is.null(FirstYearDate)) return(osm)
    #use estimate pith dates as defined above
    decayP <- exp(1/(1:(max(osm$date[curr])-FirstYearDate+1)))
    (decayP/mean(decayP))-1
    names(decayP) <- seq(FirstYearDate,max(osm$date[curr]),1)
    osm$pithDetrend[curr] <- decayP[match(osm$date[curr],names(decayP))]
  }
  return(osm)
}

getBlockDiagDetrendingMatrix <- function(osm,type,plotSubset=NULL, outdir=params$outDir,datasetname=params$datasetName) {
  if (!type%in%c("negExp","linear","pithDetrend")) {
    print("Type can be one of three options: negExp, linear or PithDetrend")
    return(NULL)
  }
  if (!type%in%colnames(osm)) {
    printf("Type entered not found in colnames: %s",type)
    return(NULL)
  }
  geo <- matrix(data = 0,nrow = nrow(osm),ncol = nrow(osm))
  for (core in unique(osm$sample)) {
    cols <- which(osm$sample==core)
    negExp <- as.matrix(osm[cols,type])
    test <- matrix(data = numeric(),nrow = nrow(negExp),ncol = nrow(negExp))
    for (x in 1:length(negExp)) {
      test[,x] <- abs(negExp[x]-negExp)
    }
    minNotZero <- min(test[row(test) == (col(test) + 1)])
    test <- (max(test)+minNotZero-test)/(max(test)+minNotZero)
    geo[cols,cols] <- test
  }
  if (is.null(plotSubset)==F) {
    par(default.par)
    par(oma=c(0,0,0,5),xpd=T)
    treeSamp <- sample(unique(osm$tree),plotSubset,F)
    jpeg(file = paste(outdir,"/",datasetname,"_detrendingMatrix","trees",paste(treeSamp,collapse="_"),".jpeg",sep=""),units = "cm",width = 25,height = 20,res = 1000,bg = NA)
    #pdf(file = paste(outdir,"/",datasetname,"_detrendingMatrix","trees",paste(treeSamp,collapse="_"),".pdf",sep=""),width = 10,height = 10,bg = NA)
    subset <- which(osm$tree%in%treeSamp)
    cols <- colorRampPalette(bias = 1,alpha=T,colors = c("white","yellow","orange","red"),interpolate = "linear")
    plot(geo[subset[order(subset)],subset[order(subset)]],col=cols,breaks=30,border=NA,main = "Negative exponential fit based on estimated pith date")
    dev.off()
    printf("\nWrote detrending matrix visualization to %s",paste(outdir,"/",datasetname,"_detrendingMatrix","trees",paste(treeSamp,collapse="_"),".jpeg",sep=""))
  }
  printf("\nReturning detrending matrix based %s with dim:",type)
  print(str(geo))
  return(geo)
}

getDetrendMatrixLoop <- function(osm,type,whichDate,plotOut=F) {
  if (!type%in%c("negExp","linear")) {
    print("Type can be either negExp or linear")
    return(NULL)
  }
  if (!whichDate%in%names(osm) | is.numeric(osm[,whichDate])==F) {
    print("need to choose a numeric value for whichDate present in osm")
    return(NULL)
  }
  n <- max(osm[,"date"])-min(osm[,whichDate])
  f <- matrix(data = 0,nrow = n,ncol = 1)
  if (type=="negExp") {
    f <- exp(1/(1:n))
    names(f) <- seq(min(osm[,whichDate])+1,max(osm[,"date"]),1)
  } else if (type=="linear") {
    f <- n:1
    names(f) <- seq(min(osm[,whichDate])+1,max(osm[,"date"]),1)
  }
  test <- matrix(data = rep(NA,(length(f)*length(f))),nrow = length(f))
  for (x in 1:length(f)) {
    test[,x] <- abs(f[x]-f)
  }
  minNotZero <- min(test[row(test) == (col(test) + 1)])
  test <- (max(test)+minNotZero-test)/(max(test)+minNotZero)
  if (plotOut) {
    par(default.par)
    par(oma=c(0,0,0,5),xpd=T)
    jpeg(file = paste(params$outDir,"/",params$datasetName,"_detrendingMatrixLoopUniversal.jpeg",sep=""),units = "cm",width = 25,height = 20,res = 1000,bg = NA)
    cols <- colorRampPalette(bias = 1,alpha=T,colors = c("white","yellow","orange","red"),interpolate = "linear")
    plot(test,col=cols,breaks=30,border=NA,main = paste(type,"fit based on",whichDate))
    dev.off()
    printf("\nWrote detrending matrix visualization to %s",paste(params$outDir,"/",params$datasetName,"_detrendingMatrixLoopUniversal.jpeg",sep=""))
  }
  printf("\nReturning detrending matrix based %s with dim:",type)
  print(str(test))
  rownames(test)<- str_pad(1:n,width = 4,pad = 0)
  colnames(test)<- str_pad(1:n,width = 4,pad = 0)
  return(test)
}

getDetrendingMatrix <- function(osm,type,whichDate,plotOut=F) {
  if (!type%in%c("negExp","linear")) {
    print("Type can be either negExp or linear")
    return(NULL)
  }
  if (!whichDate%in%names(osm) | is.numeric(osm[,whichDate])==F) {
    print("need to choose a numeric value for whichDate present in osm")
    return(NULL)
  }
  n <- max(osm[,"date"])-min(osm[,whichDate])
  f <- matrix(data = 0,nrow = n,ncol = 1)
  if (type=="negExp") {
    f <- exp(1/(1:n))
    names(f) <- seq(min(osm[,whichDate])+1,max(osm[,"date"]),1)
  } else if (type=="linear") {
    f <- n:1
    names(f) <- seq(min(osm[,whichDate])+1,max(osm[,"date"]),1)
  }
  C <- chol(matrix(kronecker(f,rev(f)),nrow = n))
  M <- C%*%t(C)
  M2 <- diag(M)-M
  M3 <- M2
  M3[lower.tri(M3)] = t(M3)[lower.tri(M3)]
  M3 <- M3-min(M3)
  M3 <- 1-scale(M3, center = apply(M3, 2, min), scale = apply(M3, 2, max) - apply(M3, 2, min))
  colnames(M3) <- str_pad(1:nrow(M3),width = 4,pad = 0)
  rownames(M3) <- str_pad(1:nrow(M3),width = 4,pad = 0)
  if (plotOut) {
    par(default.par)
    par(oma=c(0,0,0,5),xpd=T)
    jpeg(file = paste(params$outDir,"/",params$datasetName,"_detrendingMatrixUniversal.jpeg",sep=""),units = "cm",width = 25,height = 20,res = 1000,bg = NA)
    cols <- colorRampPalette(bias = 1,alpha=T,colors = c("white","yellow","orange","red"),interpolate = "linear")
    plot(M3,col=cols,breaks=30,border=NA,main = paste(type,"fit based on",whichDate))
    dev.off()
    printf("\nWrote detrending matrix visualization to %s",paste(params$outDir,"/",params$datasetName,"_detrendingMatrixUniversal.jpeg",sep=""))
  }
  printf("\nReturning detrending matrix based %s with dim:",type)
  print(str(M3))
  return(M3)
}

getIncidenceMatrix <- function(osm,term) {
  M <- matrix(data = rep(0,(nrow(osm)*length(unique(osm[,term])))),ncol = length(unique(osm[,term])),dimnames = list(rownames=1:nrow(osm),colnames=unique(osm[,term])))
  for (t in colnames(M)) {
    M[which(osm[,term]==t),t] <- 1
  }
  printf("\nReturning incidence matrix for %s with dim:",term)
  print(str(M))
  return(M)
  return(M)
}

getStartYearMatrix <- function(osm,date) {
  stYear <- aggregate(osm[,date],list(osm$sample),min)
  M <- matrix(data = rep(0,nrow(osm)),ncol = 1,dimnames = list(rownames=osm$sample,colnames=c("startYearBP")))
  for (samp in 1:nrow(stYear)) {
    cur <- which(osm$sample==stYear[samp,1])
    M[cur] <- rep(max(osm[,date])-stYear[samp,2],length(cur))
  }
  printf("\nReturning starting year matrix as calculated by the max date of all cores and the min date for that treeid with dim:")
  print(str(M))
  return(M)
}

placeSampleOnDetrendMatrix <- function(osm,whichDate) {
  stYear <- aggregate(osm[,whichDate],list(osm$sample),min) #the first year of each core sample in years BP (where BP is the most recent date for any core)
  stCore <- aggregate(osm[,"date"],list(osm$sample),min) 
  endCore <- aggregate(osm[,"date"],list(osm$sample),max) 
  n <- max(osm[,"date"])-min(osm[,whichDate])
  M <- matrix(data = rep(0,nrow(osm)*n),ncol = n)
  for (samp in 1:nrow(stYear)) {
    printf("\n%s starts at year %s on the detrend curve and extends to year %s",stCore[samp,1],stCore[samp,2]-stYear[samp,2],(stCore[samp,2]-stYear[samp,2])+(endCore[samp,2]-stCore[samp,2]))
    for (cur in which(osm$sample==stYear[samp,1])) {
      M[cur,(osm[cur,"date"]-stYear[samp,2])] <- 1
    }
  }
  
  printf("\nReturning placement of samples on detrending matrix with min %s and max %s:",min(M),max(M))
  print(str(M))
  return(M)
}

getARMatrix <- function(osm) {
  if (!type%in%c("negExp","linear","pithDetrend")) {
    print("Type can be one of three options: negExp, linear or PithDetrend")
    return(NULL)
  }
  if (!type%in%colnames(osm)) {
    printf("Type entered not found in colnames: %s",type)
    return(NULL)
  }
  printf("Making AR matrix for all")
  geo <- matrix(data = 0,nrow = nrow(osm),ncol = nrow(osm))
  for (core in unique(osm$sample)) {
    cols <- which(osm$sample==core)
    negExp <- as.matrix(osm[cols,type])
    test <- matrix(data = numeric(),nrow = nrow(negExp),ncol = nrow(negExp))
    for (x in 1:length(negExp)) {
      test[,x] <- abs(negExp[x]-negExp)
    }
    minNotZero <- min(test[row(test) == (col(test) + 1)])
    test <- (max(test)+minNotZero-test)/(max(test)+minNotZero)
    geo[cols,cols] <- test
  }
  #plot(geo,breaks = 20,border=NA)
  return(geo)
}

checkNorm <- function(x,main="Check normality") {
  print("Remove NAs before checking for normality")
  x <- x[which(is.na(x)==F)]
  print(paste(length(x),"resulting rows"))
  par(mfrow=c(2,1),oma=c(0,0,0,0),mar=c(2,2,1.1,0))
  hist(x,freq = F,main = paste(main,"-","hist"),breaks=50)
  normal <- rnorm(n = length(x),mean = mean(x),sd = sd(x))
  lines(density(normal),type = "l",col="firebrick")
  plot(x = sort(normal,decreasing = F),sort(normal,decreasing = F),col="firebrick",type="l",ylim=c(min(c(x,normal)),max(c(x,normal))),main=paste(main,"-","QQ"))
  points(sort(normal,decreasing = F),sort(x,decreasing = F),cex=.5)
  if (length(x)>5000) {
    print("Downsampling to 5000 for shapiro.test")
    x <- x[sample(x = x,size = 5000,replace = F)]
  }
  print(shapiro.test(x))
}

plotDetrendOne <- function(osm,pdfFile=NULL) {
  if (is.null(pdfFile)==F) pdf(file=pdfFile,width = 5,height=12)
  par(mfrow=c(6,1),oma=c(2,0,5,0),mar=c(2,4,3.1,.1))
  ind=1
  for (core in unique(osm$sample)) {
    print(core)
    if (ind>6) ind=1
    l <- which(osm$sample==core & is.na(osm$width)==F)
    lm.curr <- lm(osm$width[l]~osm$pithDetrend[l])
    lm.log <- lm(osm$widthLog[l]~osm$pithDetrend[l])
    lm.sqrt <- lm(sqrt(osm$width[l])~osm$pithDetrend[l])
    d <- plot(osm$date[l],osm$pithDetrend[l], type = "l",col="red",ylab="cm",xlim=c(min(osm$date[which(osm$tree%in%osm$tree[l])]),max(osm$date[l])),ylim=c(-2,max(osm$width[l],na.rm = T)),main=core,xpd=T)
    abline(lm(osm$width[l]~osm$date[l]),col="gray")
    lines(x = osm$date[l],y = osm$width[l],col="#0000FF80",type = "l")
    lines(x = osm$date[l],y = (mean(osm$width[l]) + lm.log$residuals),col="#FF149380",type = "l")
    lines(x = osm$date[l],y = mean(osm$width[l]) + lm.sqrt$residuals,col="#bc7d3980",type = "l")
    legend("bottomleft",legend = c("pithDetrend","raw","log","sqrt"),col = c("red","#0000FF80","#FF149380","#bc7d3980"),lty = 1,cex=.5)
    ind=ind+1
  }
  if (is.null(pdfFile)==F) dev.off()
  par(default.par)
}

plotDetrendLM <- function(osm,osm.detrend,square=T,pdfFile=NULL) {
  if (is.null(pdfFile)==F) pdf(file=pdfFile,width = 5,height=12)
  par(mfrow=c(6,1),oma=c(2,0,5,0),mar=c(2,4,3.1,.1))
  ind=1
  if ("widthAdj"%in%names(osm.detrend)==F) {
    print("MUST have a column called widthAdj! This is supposed to be for detrended")
    return(F)
  }
  for (core in unique(osm$sample)) {
    print(core)
    if (ind>6) ind=1
    l <- which(osm$sample==core & is.na(osm$width)==F)
    tree <- unique(osm$tree[which(osm$sample==core)])
    treeL <- which(osm.detrend$tree==tree)
    d <- plot(osm$date[l],osm$pithDetrend[l], type = "l",col="red",ylab="cm",xlim=c(min(osm$date[which(osm$tree%in%osm$tree[l])]),max(osm$date[l])),ylim=c(-2,max(osm$width[l],na.rm = T)),main=core,xpd=T)
    if (square) {
      abline(lm(osm$width[l]~osm$date[l]),col="gray")
      abline(lm((osm.detrend$widthAdj[treeL])^2~osm.detrend$date[treeL]),col="black")
      lines(x = (osm.detrend$date[treeL]),y = (osm.detrend$widthAdj[treeL])^2,col="#6fac5d80",type = "l")
      rawfit <- "fitRaw"
      lmname <- "fitLMDetrendSquared"
      lmfit <- "LMDetrendSquared"
    } else {
      abline(lm(sqrt(osm$width[l])~osm$date[l]),col="gray")
      abline(lm(osm.detrend$widthAdj[treeL]~osm.detrend$date[treeL]),col="black")
      lines(x = osm.detrend$date[treeL],y = osm.detrend$widthAdj[treeL],col="#6fac5d80",type = "l")
      rawfit <- "fitSqrtRaw"
      lmname <- "fitLMDetrend"
      lmfit <- "LMDetrend"}
    lines(x = osm$date[l],y = osm$width[l],col="#0000FF80",type = "l")
    lines(x = osm$date[l],y = sqrt(osm$width[l]),col="#bc7d3980",type = "l")
    legend("bottomleft",legend = c("pithDetrend",rawfit,lmname,"raw","sqrtRaw",lmfit),col = c("red","grey","black","#0000FF80","#bc7d3980","#6fac5d80"),lty = 1,cex=.5,ncol = 3)
    ind=ind+1
  }
  if (is.null(pdfFile)==F) dev.off()
  par(default.par)
}

plotFromLM <- function(osm.detrend,xterm="date",yterm,colorTerm=NULL,scaleCol=1,cex.size=.1,forcePt=F,pdfFile=NULL,colortitle=NULL,title=NULL) {
  if (yterm%in%names(osm.detrend)==F) {
    printf("No term called %s in osm.detrend!",yterm)
  }
  if (xterm%in%names(osm.detrend)==F) {
    printf("No term called %s in osm.detrend!",xterm)
  }
  if (is.null(colortitle)) colortitle <- colorTerm
  pal <- NULL
  if (is.null(pdfFile)==F) pdf(file=pdfFile,width = 7,height=5)
  if (forcePt & (grepl(pattern = ":",x = yterm) | nrow(unique(osm.detrend[,c("tree",xterm)]))>length(unique(osm.detrend$tree)))) {
    if (grepl(pattern = ":",x = yterm)) {
      x <- strsplit(yterm,":")[[1]][1]
      y <- strsplit(yterm,":")[[1]][2]
      printf("Using %s for x and %s for y",x,yterm)
    } else {
      y <- yterm
      x <- xterm
    }
    agg <- unique(osm.detrend[,c("tree",yterm,x,colorTerm)])
    if (is.null(colorTerm)) {
      colorTerm <- "unique"
      col <- uniqueCols(length(unique(agg$tree)))
      cols <- data.frame(tree=unique(agg$tree),col,stringsAsFactors = F)
      agg$col <- cols[match(agg$tree,cols$tree),2] 
    } else {
      heatmap <- getHeatMapCol(agg[,colorTerm]*scaleCol)
      agg$col <- heatmap$cols
      pal <- heatmap$pal
    }
    if (is.null(title)) title <- paste("Random effect of ",sub(pattern = "E",replacement = "",x = yterm),sep="")
    if (is.null(pal)==F) par(oma=c(0,0,0,3))
    plot(0,ylab=yterm,xlab=x,xlim=c(min(as.numeric(agg[,x])),max(as.numeric(agg[,x]))),ylim=c(min(agg[,yterm]),max(agg[,yterm])),main=title,xpd=T)
    for (tree in agg$tree) {
      lines(as.numeric(agg[agg$tree==tree,x]),agg[agg$tree==tree,yterm],col=agg[agg$tree==tree,"col"],lwd=.1,pch=19,type="l",cex=cex.size,main=title,xpd=T)
    }
    if (is.null(pal)==F) subplot(fun = color.bar(pal,min = min(osm.detrend[,colorTerm]),title=colortitle,max = max(osm.detrend[,colorTerm])),hadj = 1,vadj = 0,x = c(par("usr")[2]+sd(osm.detrend[,x])/30,par("usr")[2]+sd(osm.detrend[,x])/3),y = c(min(osm.detrend[,yterm]),max(osm.detrend[,yterm])))
  } else {
    if (is.null(colorTerm)) {
      agg <- unique(osm.detrend[,c("tree",yterm,xterm)])
      colorTerm <- "unique"
      agg$col <- uniqueCols(length(unique(agg$tree)))
    } else {
      agg <- unique(osm.detrend[,c("tree",yterm,xterm,colorTerm)])
      heatmap <- getHeatMapCol(agg[,colorTerm]*scaleCol)
      agg$col <- heatmap$cols
      pal <- heatmap$pal
    }
    if (is.null(title)) title <- paste("Random effect of ",sub(pattern = "E",replacement = "",x = yterm)," for ",xterm,sep="")
    if (is.null(pal)==F) par(oma=c(0,0,0,3))
    plot(agg[,xterm],agg[,yterm],col=agg$col,ylab=yterm,xlab=xterm,cex=cex.size,pch=19,main=title,xpd=T)
    if (is.null(pal)==F) subplot(fun = color.bar(pal,min = min(osm.detrend[,colorTerm]),title=colortitle,max = max(osm.detrend[,colorTerm])),hadj = 1,vadj = 0,x = c(par("usr")[2]+sd(osm.detrend[,xterm])/30,par("usr")[2]+sd(osm.detrend[,xterm])/3),y = c(min(osm.detrend[,yterm]),max(osm.detrend[,yterm])))
  }
  if (is.null(pdfFile)==F) dev.off()
  par(default.par)
}

detrendAndPlot <- function(osm,flex,range=c(min(osm$date),max(osm$date))) {
  plot(0,0,xlim = range,ylim = c(-2,2),xlab="Date",ylab=paste("Core mean plus spline =",flex,"residuals"))
  cols <- rainbow(length(unique(osm$sample)))
  for (core in 1:length(unique(osm$sample))) {
    l <- which(osm$sample==unique(osm$sample)[core])
    l <- l[which((is.na(osm[l,]$width)==F))]
    spl <- smooth.spline(x = list(x=osm$date[l],y = osm$width[l]),spar = flex)
    lm.curr <- lm(osm$width[l]~spl$y)
    #expon <- mean(osm$width[l])+spl$y[1]^(-.1*(1:length(osm$date[l])))
    #lm.expon <- lm(log(osm$width[l]+.0000000000001)~expon)
    lm.log <- lm(log(osm$width[l]+.001)~spl$y)
    # if want to include the core mean: lm.curr$coefficients[1]+mean(lm.curr$fitted.values)+
    lines(x = osm$date[l],y = lm.log$residuals,col=cols[core],type = "l")
  }
  par(default.par)
  return(T)
}

addSizeToDetrend <- function(osm.detrend) {
  if ("dbhcm"%in%names(osm.detrend)==F) {
    printf("osm.detrend does not have a variable called dbhcm!")
    return(osm.detrend)
  }
  osm.detrend$Size <- numeric(nrow(osm.detrend))
  for (tree in unique(osm.detrend$tree)) {
    curr <- osm.detrend[which(osm.detrend$tree==tree),]
    printf("\nTree %s dbhcm: %s",tree,mean(curr$dbhcm))
    ordD <- order(curr$date,decreasing = T)
    for (x in 1:length(ordD)) {
      osm.detrend$Size[which(osm.detrend$`DateAsFactor:tree`==curr[ordD[x],"DateAsFactor:tree"])] <- (curr$dbhcm[ordD[x]])-(2*(sum(curr$width[ordD[1:x]])/10))
    }
    #hist(osm.detrend$Size[which(osm.detrend$tree==tree)],main=tree,breaks=20)
  }
  return(osm.detrend)
}

addEffectsFromSommer <- function(osm.detrend,ans1) {
  for (ran in names(ans1$U)) {
    names <- strsplit(ran,":")[[1]]
    treeYear <- data.frame(eff=ans1$U[[ran]][[1]],var=names(ans1$U[[ran]][[1]]),stringsAsFactors = F)
    if (length(names)>1) {
      if (names[1]%in%unique(osm.detrend$DateAsFactor) & names[2]=="tree") {
        osm.detrend[,paste(ran,"E",sep="")] <- treeYear[match(osm.detrend[,names[2]],treeYear[,2]),1]
      } else {
        splitName <- str_split_fixed(treeYear[,2], ":", 2)
        treeYear[,names[1]] <- str_replace_all(splitName[,1],names[1],"")
        treeYear[,names[2]] <- str_replace_all(splitName[,2],names[2],"")
        osm.detrend[,paste(ran,"E",sep="")] <- treeYear[match(paste(osm.detrend[,names[1]],osm.detrend[,names[2]],sep=":"),paste(treeYear[,names[1]],treeYear[,names[2]],sep=":")),"e"]
      }
    } else {
      treeYear[,names[1]] <- str_replace_all(treeYear$var,names[1],"")
      osm.detrend[,paste(ran,"E",sep="")] <- treeYear[match(osm.detrend[,names[1]],treeYear[,names[1]]),"e"]
    }
  }
  osm.detrend$residuals <- ans1$residuals
  if (all(c("treeE","DateAsFactorE","DateAsFactor:treeE")%in%names(osm.detrend))) osm.detrend$widthAdj <- ((apply(cbind(osm.detrend[,"treeE"],osm.detrend[,"DateAsFactorE"],osm.detrend[,"DateAsFactor:treeE"]),MARGIN = 1,sum))+ans1$Beta$Estimate[which(ans1$Beta$Effect=="(Intercept)")])
  return(osm.detrend)
}
##Functions to interact with azure postgresDB

getDBColumnDF <- function(outDir) {
  system(paste("source /Users/kelly.swarts/.bash_profile; PGPASSWORD=piceaabies psql -h trg-srv.postgres.database.azure.com -d trgdb -U picea@trg-srv -c \"COPY (select table_name, count(*) as column_count from information_schema.columns where table_schema = 'public' GROUP by table_name order by column_count desc) TO STDOUT WITH CSV HEADER\" > ",outDir,"/TableColumnNum.csv",sep=""))
  db_col_num <- read.csv(paste(outDir,"/TableColumnNum.csv",sep=""),header = T,as.is = T)
}

getMapPointCoords <- function(mappointid,outDir=getwd(),dbworks=F,keepFile=F) {
  mo <- getDataFromDB(query = paste("SELECT wgs84collect FROM MapPoint WHERE mappointid =",mappointid,sep=" "))
  if (is.na(mo[1,1])) return(NULL)
  coords <- fromJSON(txt = mo[1,1])
  df <- as.data.frame(fromJSON(txt = coords[[1]]))
  for (r in 2:length(coords)) {
    df <- rbind(df,as.data.frame(fromJSON(txt = coords[[r]])))
  }
  return(df)
}

getSpecies <- function(dbworks=F) {
  mo <- getDataFromDB(query = paste("SELECT * FROM species",sep=" "))
  return(mo)
}

#region can be "Europe_all","Europe_spruce","N_America_all","Americas_maize" or a dataframe with "x" and "y" min and max coordinates
MapPlotLocations <- function(plotids,plotname = NA,region=NULL,elevation=T,ptCol="#c85044",ptSize=2,labels=T,resolutionMeters=1000,pdfFile=NULL,buffer=0,colorByYear=F,kmlFile=NULL) {
  wgs <- getDataFromDB(query = paste("SELECT p.plotid,p.centerpoint,p.country,p.collected,p.altname,m.median_wgs84n as y,m.median_wgs84e as x,m.median_alt as altitude from public.plot as p, public.mappoint as m where p.centerpoint = m.mappointid AND plotid in (",paste(plotids,collapse=","),") order by p.plotid",sep=""))
  if (length(which(is.na(wgs$x)))>0) print(paste("No spatial data for plots:",paste(wgs$plotid[which(is.na(wgs$x))],collapse=",")))
  wgs <- wgs[which(is.na(wgs$x)==F),]
  if (nrow(wgs)<1) return(NULL)
  if (is.null(kmlFile)==F) convertToKML(df=wgs,idCol="altname",keepCol=c("plotid","country","altitude"),outDir = dirname(kmlFile),outBase = basename(kmlFile))
  wgs$collected <- lapply(wgs$collected,FUN=function(x){strsplit(x = x,split = '-',fixed = T)[[1]][1]})
  crs <- "+proj=longlat +datum=WGS84 +no_defs"
  newmap <- getMap(resolution = "high")
  newmap <- spTransform(newmap, CRS = CRS(projargs = crs))
  focus <- data.frame("x"=((max(wgs$x)-min(wgs$x))*.1)+buffer,"y"=((max(wgs$y)-min(wgs$y))*.1)+buffer)
  bounds <- data.frame("x"=c(min(wgs$x)-focus$x, max(wgs$x)+focus$x),"y"=c(min(wgs$y)-(focus$y), max(wgs$y)+(focus$y)))
  if (is.null(region)==F) {
    if (is.data.frame(region) & all(colnames(region)%in%c("x","y"))) {bounds <- region
    }else if (region=="Europe_spruce") {bounds <- data.frame("x"=c(-5,30),"y"=c(40,62))
    }else if (region=="Europe_all") {bounds <- data.frame("x"=c(-12,45),"y"=c(35,71))
    }else if (region=="N_America_all") {bounds <- data.frame("x"=c(-167,-50),"y"=c(16,80))
    }else if (region=="Americas_maize") {bounds <- data.frame("x"=c(-130,-30),"y"=c(-55,50))}
  }
  z <- max(4,round(14-((max(bounds$x)-min(bounds$x)+(max(bounds$y)-min(bounds$y)))),digits = 0))
  correction <- 2-((90-abs(mean(bounds$y)))/90)
  xovery <- (max(bounds$x)-min(bounds$x))/((max(bounds$y)-min(bounds$y))*correction) # correction because globe
  height <- 12
  width <- 12
  if (xovery>=1) {height <- round(12*(((max(bounds$y)-min(bounds$y))*correction)/(max(bounds$x)-min(bounds$x))),0)
  } else {width <- round(12*xovery,0)}
  if (is.null(pdfFile)==F) pdf(file=pdfFile,width = width,height=height)
  par(mar=c(5, 4, 4, 5) + 0.1)
  elev=NA
  if (elevation) {
    elev <- get_elev_raster(locations = bounds,clip = "locations", z = z,prj = crs)
    if (abs(ceiling((elev@data@max-elev@data@min)/resolutionMeters))<2) {
      print(paste("Changing resolution from",resolutionMeters,"to",resolutionMeters/10))
      resolutionMeters <- resolutionMeters/10
    }
    plot("", main=plotname,xlim=bounds$x,ylim=bounds$y,xlab="WGS84E",ylab="WGS84N",xaxs="i", yaxs="i")
    if (elev@data@min<0) {plot(elev,col = c(colorRampPalette(colors = c("#0c75a5","#e5f3ff"))(abs(ceiling(elev@data@min/resolutionMeters))),colorRampPalette(colors = c("#f1edda","#316525"))(abs(ceiling(elev@data@max/resolutionMeters)))),add=T)
    } else {plot(elev,col = colorRampPalette(colors = c("#f1edda","#316525"))(abs(ceiling((elev@data@max-elev@data@min)/resolutionMeters))),add=T)}
    plot(newmap,xlim=bounds$x,ylim=bounds$y, asp = 1,add=T,mar=c(0,0,0,0),xpd=F)
  } else {
    plot(newmap, main=plotname,xlim=bounds$x,ylim=bounds$y,xlab="WGS84E",ylab="WGS84N",xaxs="i", yaxs="i")
  }
  points(wgs$x,wgs$y,bg=ptCol,col="black",pch = 21, cex=ptSize)
  if (labels) text(wgs$x,wgs$y,col="firebrick",wgs$altname,pos=4,cex=.7)
  if (is.null(pdfFile)==F) dev.off()
  par(mar=c(5, 4, 4, 2) + 0.1)
  return(list("wgs"=wgs,"bounds"=bounds,"elev"=elev,"width"=width,"height"=height))
}

#region can be "Europe_all","Europe_spruce","N_America_all","Americas_maize" or a dataframe with "x" and "y" min and max coordinates
MapFromDF <- function(wgs,xCol,yCol,labelCol,plotname="",region=NULL,elevation=T,ptCol="#c85044",ptSize=2,colorCol=NULL,labels=T,resolutionMeters=1000,pdfFile=NULL,buffer=0,colorByYear=F,KMLFile=NULL) {
  if (all(c(xCol,yCol,labelCol)%in%colnames(wgs))==F) {
    print("Couldn't find xCol, yCol or label in wgs column names!")
    return(NULL)
  } else {
    wgs$x <- as.numeric(wgs[,which(names(wgs)==xCol)])
    wgs$y <- as.numeric(wgs[,which(names(wgs)==yCol)])
    wgs <- wgs[which(is.na(wgs$x)==F),]
    wgs <- wgs[which(is.na(wgs$y)==F),]
  }
  if (nrow(wgs)<1) return(NULL)
  if (is.null(KMLFile)==F) convertToKML(df=wgs,idCol="altname",keepCol=c("plotid","country","altitude"),outDir = dirname(kmlFile),outBase = basename(kmlFile))
  crs <- "+proj=longlat +datum=WGS84 +no_defs"
  newmap <- getMap(resolution = "high")
  newmap <- spTransform(newmap, CRS = CRS(projargs = crs))
  focus <- data.frame("x"=((max(wgs$x,na.rm = T)-min(wgs$x,na.rm = T))*.1)+buffer,"y"=((max(wgs$y,na.rm = T)-min(wgs$y,na.rm = T))*.1)+buffer)
  bounds <- data.frame("x"=c(min(wgs$x,na.rm = T)-focus$x, max(wgs$x,na.rm = T)+focus$x),"y"=c(min(wgs$y,na.rm = T)-(focus$y), max(wgs$y,na.rm = T)+(focus$y)))
  if (is.null(region)==F) {
    if (is.data.frame(region) & all(colnames(region)%in%c("x","y"))) {bounds <- region
    }else if (region=="Europe_spruce") {bounds <- data.frame("x"=c(-5,30),"y"=c(40,62))
    }else if (region=="Europe_all") {bounds <- data.frame("x"=c(-12,45),"y"=c(35,71))
    }else if (region=="N_America_all") {bounds <- data.frame("x"=c(-167,-50),"y"=c(16,80))
    }else if (region=="Americas_maize") {bounds <- data.frame("x"=c(-130,-30),"y"=c(-55,50))}
  }
  z <- max(4,round(14-((max(bounds$x)-min(bounds$x)+(max(bounds$y)-min(bounds$y)))),digits = 0))
  correction <- 2-((90-abs(mean(bounds$y)))/90)
  xovery <- (max(bounds$x)-min(bounds$x))/((max(bounds$y)-min(bounds$y))*correction) # correction because globe
  height <- 12
  width <- 12
  if (xovery>=1) {height <- round(12*(((max(bounds$y)-min(bounds$y))*correction)/(max(bounds$x)-min(bounds$x))),0)
  } else {width <- round(12*xovery,0)}
  if (is.null(pdfFile)==F) svg(file=pdfFile,width = width,height=height)
  par(mar=c(5, 4, 4, 5) + 0.1)
  elev=NA
  if (elevation) {
    elev <- get_elev_raster(locations = bounds,clip = "locations", z = z,prj = crs)
    if (abs(ceiling((elev@data@max-elev@data@min)/resolutionMeters))<2) {
      print(paste("Changing resolution from",resolutionMeters,"to",resolutionMeters/10))
      resolutionMeters <- resolutionMeters/10
    }
    plot("", main=plotname,xlim=bounds$x,ylim=bounds$y,xlab="WGS84E",ylab="WGS84N",xaxs="i", yaxs="i")
    if (elev@data@min<0) {plot(elev,col = c(colorRampPalette(colors = c("#0c75a5","#e5f3ff"))(abs(ceiling(elev@data@min/resolutionMeters))),colorRampPalette(colors = c("#f1edda","#316525"))(abs(ceiling(elev@data@max/resolutionMeters))+1)),add=T)
    } else {plot(elev,col = colorRampPalette(colors = c("#f1edda","#316525"))(abs(ceiling((elev@data@max-elev@data@min)/resolutionMeters))),add=T)}
    plot(newmap,xlim=bounds$x,ylim=bounds$y, asp = 1,add=T,mar=c(0,0,0,0),xpd=F)
  } else {
    plot(newmap, main=plotname,xlim=bounds$x,ylim=bounds$y,xlab="WGS84E",ylab="WGS84N",xaxs="i", yaxs="i")
  }
  if (is.null(colorCol)==F) {
    wgs[which(wgs[,colorCol]==""),colorCol] <- NA
    cols <- wgs[,colorCol]
    cols[which(is.na(wgs[,colorCol]))] <- "#000000"
    points(wgs$x,wgs$y,bg=cols,col="black",pch = 21, cex=ptSize)
  } else {points(wgs$x,wgs$y,bg=ptCol,col="black",pch = 21, cex=ptSize)}
  if (labels) text(wgs$x,wgs$y,col="firebrick",wgs[,labelCol],pos=4,cex=.7)
  if (is.null(pdfFile)==F) dev.off()
  par(mar=c(5, 4, 4, 2) + 0.1)
  return(list("wgs"=wgs,"bounds"=bounds,"elev"=elev,"width"=width,"height"=height))
}

MapForMapPoint <- function(mo,whichMP) {
  par(xpd=T)
  ###########DEFS#############
  #good colors: c("#72a555","#d36337","#842f2f","#cda448","#8f692e")
  subspecies <- which(names(col.species)%in%mo$speciesid)
  col.species <- col.species[subspecies]
  names.species <- names.species[subspecies]
  dbh <- log(1+mo$dbhcm/10)
  
  #set up a circular plot
  SetUpCircularPlot()
  
  # Add points
  namedT <- mo[which(is.na(mo$treeid)==F),"treeid"]
  #namedT <- paste(substr(as.character(as.numeric(namedT)), 1, nchar(as.character(as.numeric(namedT))[1])-3),substr(as.character(as.numeric(namedT)), nchar(as.character(as.numeric(namedT))[1])-1, nchar(as.character(as.numeric(namedT))[1])),sep="-")
  namedT <- substr(as.character(as.numeric(namedT)), nchar(as.character(as.numeric(namedT))[1])-1, nchar(as.character(as.numeric(namedT))[1]))
  cols <- rep("#ff1493",nrow(mo))
  for (taxa in names(col.species)) {cols[which(mo$speciesid==taxa)] <- col.species[taxa]}
  cols[which(is.na(mo$treeid)==T)] <- paste(cols[which(is.na(mo$treeid)==T)],"CC",sep="")
  bgs <- rep("#808080",nrow(mo))
  for (status in names(col.status)) {bgs[which(mo$status==status)] <- col.status[status]}
  bgs[which(is.na(mo$treeid)==T)] <- paste(bgs[which(is.na(mo$treeid)==T)],"CC",sep="")
  points(with(mo, get.coords(compassaz, compasshd, 0, 0)),pch=21,col=bgs,bg = cols,lwd=2,cex=dbh)
  if (length(namedT)>0) text(with(mo[which(is.na(mo$treeid)==F),], get.coords(compassaz, compasshd, 0, 0)), labels =  namedT,col = bgs[which(is.na(mo$treeid)==F)],adj = c(0,2),cex=.75)
  #legend("bottomright",legend = c(names.species,names(col.status)),pch = 21,pt.bg = c(col.species,"#ff1493",rep("white",length(col.status))),col = c(rep("white",length(names.species)),col.status),pt.lwd = 2)
  legend("bottomright",title = "Species",legend = c(names.species),pch = 21,pt.bg = c(col.species,"#ff1493"),col = "white",pt.lwd = .5,cex=.8)
  legend("topright",title = "Status",legend = names(col.status),pch = 21,pt.bg = rep("white",length(col.status)),col = col.status,pt.lwd = 2,cex=.8)
  mp <- getMapPointCoords(mappointid = whichMP)
  result = tryCatch({
    text(x = -35,y = 25,pos = 4,labels = paste("Plot ID: ",unique(mo$plotid),"\nMappoint ID: ",unique(mo$mappointid),"\nWGS84N: ",format(round(median(mp$wgs84n), 6), nsmall = 6),"\nWGS84N: ",format(round(median(mp$wgs84e), 6), nsmall = 6),"\naltitude: ",format(round(median(mp$alt), 2), nsmall = 2),sep=""),cex=.9)
  }, warning = function(w) {
    text(x = -35,y = 25,pos = 4,labels = paste("Plot ID: ",unique(mo$plotid),"\nMappoint ID: ",unique(mo$mappointid),"\nWGS84N: NA\nWGS84N: NA\naltitude: NA",sep=""),cex=.9)
    print(e)
  }, error = function(e) {
    text(x = -35,y = 25,pos = 4,labels = paste("Plot ID: ",unique(mo$plotid),"\nMappoint ID: ",unique(mo$mappointid),"\nWGS84N: NA\nWGS84N: NA\naltitude: NA",sep=""),cex=.9)
    print(e)
  }, finally = {
    text(x = -35,y = 25,pos = 4,labels = paste("Plot ID: ",unique(mo$plotid),"\nMappoint ID: ",unique(mo$mappointid),"\nWGS84N: NA\nWGS84N: NA\naltitude: NA",sep=""),cex=.9)
  })
}

mapPlot <- function(mo,plotid,outpdf=NULL,alphaHexForUnnamed="CC",namedOnly=F) {
  if (is.null(outpdf)==F) pdf(file = outpdf,width = 7,height = 7,useDingbats = F)
  par(xpd=T)
  mo$speciesid[is.na(mo$speciesid)] <- "UNK spp."
  subspecies <- which(names(col.species)%in%unique(mo$speciesid))
  col.species <- col.species[subspecies]
  names.species <- names.species[subspecies]
  
  SetUpCircularPlot()
  # Add points
  namedT <- mo[which(is.na(mo$treeid)==F),"treeid"]
  #namedT <- paste(substr(as.character(as.numeric(namedT)), 1, nchar(as.character(as.numeric(namedT))[1])-3),substr(as.character(as.numeric(namedT)), nchar(as.character(as.numeric(namedT))[1])-1, nchar(as.character(as.numeric(namedT))[1])),sep="-")
  #namedT <- substr(as.character(as.numeric(namedT)), nchar(as.character(as.numeric(namedT))[1])-1, nchar(as.character(as.numeric(namedT))[1]))
  namedT <- as.character(as.numeric(namedT))
  cols <- rep("#ff1493",nrow(mo))
  for (taxa in names(col.species)) {cols[which(mo$speciesid==taxa)] <- col.species[taxa]}
  cols[which(is.na(mo$treeid)==T)] <- paste(cols[which(is.na(mo$treeid)==T)],alphaHexForUnnamed,sep="") #CC is 80, 4D is 40
  bgs <- rep("#808080",nrow(mo))
  for (status in names(col.status)) {bgs[which(mo$status==status)] <- col.status[status]}
  bgs[which(is.na(mo$treeid)==T)] <- paste(bgs[which(is.na(mo$treeid)==T)],alphaHexForUnnamed,sep="")
  if (is.null(mo)==F) {
    dbh <- log(1+mo$dbhcm/10)
    if (namedOnly) {
      points(x = mo[,'x'],y = mo[,'y'],pch=21,col=bgs,bg = cols,lwd=2,cex=dbh)
    } else {
      points(x = mo$x,y = mo$y,pch=21,col=bgs,bg = cols,lwd=2,cex=dbh)
    }
    if (length(namedT)>0) text(with(mo[which(is.na(mo$treeid)==F),], get.coords(compassaz, compasshd, 0, 0)), labels =  namedT,col = bgs[which(is.na(mo$treeid)==F)],adj = c(0,2),cex=.75)
    legend("bottomright",title = "Species",legend = c(names.species),pch = 21,pt.bg = c(col.species,"#ff1493"),col = "white",pt.lwd = .5,cex=.8)
    legend("topright",title = "Status",legend = names(col.status),pch = 21,pt.bg = rep("white",length(col.status)),col = col.status,pt.lwd = 2,cex=.8)
    mp <- getMapPointCoords(mappointid = min(mo$mappointid))
    labelString <- paste("Plot ID: ",unique(mo$plotid),"\nCenter Mappoint ID: ",unique(mo$mappointid),"\nWGS84N: ",format(round(median(mp$wgs84n), 6), nsmall = 6),"\nWGS84N: ",format(round(median(mp$wgs84e), 6), nsmall = 6),"\naltitude: ",format(round(median(mp$alt), 2), nsmall = 2),sep="")
    if (namedOnly) {labelString <- paste(labelString,"\nOnly sampled trees!",sep="")}
    text(x = -35,y = 25,pos = 4,labels = labelString,cex=.9)
  }
  if (is.null(outpdf)==F) dev.off()
}

processMappingMO <- function(plotids) {
  print(paste("Processing mappings for plotids: ",plotids))
  plots <- paste(as.numeric(plotids),collapse = " OR p.plotid=")
  mo <- getDataFromDB(query = paste("SELECT p.recorder, m.* FROM plot p INNER JOIN mappedobject m ON p.plotid = m.plotid where p.plotid=",plots,sep=""))
  if (is.null(mo)) return(mo)
  print(paste("Inital mapped objects from DB: ",nrow(mo)))
  cleanmo <- cleanMappoints(mo)
  if (is.null(cleanmo)) return(cleanmo)
  print(paste("Mapped object after cleaning: ",nrow(mo)))
  cleanmo <- consensusCoordinates(mo = cleanmo)
  if (is.null(cleanmo)) return(cleanmo)
  print(paste("Mapped object after generating consensus x,y,z for plot: ",nrow(mo)))
  return(cleanmo)
}

processMapping <- function(osm,mapFile=NULL) {
  plots <- paste(as.numeric(unique(osm$plot)),collapse = " OR p.plotid=")
  mo <- getDataFromDB(query = paste("SELECT p.recorder, m.* FROM plot p INNER JOIN mappedobject m ON p.plotid = m.plotid where p.plotid=",plots,sep=""))
  col_orig <- c(rep("p",db_col_num[which(db_col_num$table_name=="plot"),2]),rep("m",db_col_num[which(db_col_num$table_name=="mappedobject"),2]))
  cleanmo <- cleanMappoints(mo)
  mat <- match(osm$tree,cleanmo$treeid)
  osm[,c("status","dbhcm","mapper","speciesid","x","y","z")] <- cleanmo[mat,c("status","dbhcm","mapper","speciesid","x","y","z")]
  return(list(osm=osm,mo=cleanmo))
}

addPlotData <- function(osm,addCols=c("slope","aspect","mapper","coreaspect","samplingheightcm","coreperson"),outDir) {
  plots <- paste(as.numeric(unique(osm$plot)),collapse = " OR c.plotid=")
  query <- paste("SELECT * FROM plot p INNER JOIN tree t ON p.plotid = t.plotid RIGHT JOIN mappedobject m ON t.treeid = m.treeid INNER JOIN core c ON t.treeid=c.treeid where p.plotid=",plots,sep="")
  mo <- getDataFromDB(query = query)
  #col_orig <- c(rep("p",db_col_num[which(db_col_num$table_name=="plot"),2]),rep("t",db_col_num[which(db_col_num$table_name=="tree"),2]),rep("m",db_col_num[which(db_col_num$table_name=="mappedobject"),2]),rep("c",db_col_num[which(db_col_num$table_name=="core"),2]))
  #bindCol <- paste(col_orig,colnames(dbRaw),sep = "_")
  #keepCols <- which(bindCol%in%c("p_plotid","t_treeid","c_coreid","p_slope","p_aspect","p_altitude","t_collected.1","p_country","p_recorder","p_person_dna","p_altname","m_mappointid","m_mapper","m_sampletime","m_status","m_dbhcm.1","m_speciesid.1","m_compasssd","m_compasshd","m_compassh","m_compassdeg","m_compassaz","c_personid","c_samplingheightcm","c_coreaspect")==T)
  #db <- dbRaw[,keepCols]
  #colnames(db)[which(colnames(db)=="personid")] <- "coreperson"
  #colnames(db)[which(colnames(db)=="dbhcm.1")] <- "dbhcm"
  #colnames(db)[which(colnames(db)=="speciesid.1")] <- "speciesid"
  #colnames(db)[which(colnames(db)=="collected.1")] <- "tree_collected"
  mat <- match(osm$sample,mo$coreid) 
  osm[,addCols] <- mo[mat,addCols]
  return(osm)
}

potentialDups <- function(mo,dbh = 8.3,hd = .3,az = 10.1,outdir) {
  close <- list()
  for (plot in unique(mo$plotid)) {
    for (i in which(mo$plotid==plot)) {
      if (TRUE%in%is.na(mo[i,c("dbhcm","compasshd","compassaz")])) {
        print(paste("Problem! Skip",mo[i,"mappedobjectid"]))
        next
      }
      currClose <- i
      for (j in (i+1):max(which(mo$plotid==plot))) {
        if (TRUE%in%is.na(mo[j,c("dbhcm","compasshd","compassaz")])) {
          print(paste("Problem! Skip",mo[j,"mappedobjectid"]))
          next
        }
        if ((mo$dbhcm[i] < mo$dbhcm[j]+dbh) & (mo$dbhcm[i] > mo$dbhcm[j]-dbh) & (mo$compasshd[i] < mo$compasshd[j]+hd) & (mo$compasshd[i] > mo$compasshd[j]-hd) & (mo$compassaz[i] < mo$compassaz[j]+az) & (mo$compassaz[i] > mo$compassaz[j]-az)) currClose <- c(currClose,j)
      }
      #print(currClose)
      if (length(currClose)>1) close[[as.character(i)]] <- currClose
    }
  }
  for (d in close) {
    print(mo[d,])
    write.table(x=mo[d,],file = paste(outdir,"/possibleDupMappedObjects.txt",sep = ""),append = T)
  }
  return(close)
}

get.coords <- function(a, d, x0, y0) {
  a <- ifelse(a <= 90, 90 - a, 450 - a)
  data.frame(x = x0 + d * cos(a / 180 * pi), 
             y = y0+ d * sin(a / 180 * pi))
}

get.coords.TRG <- function(coords, x0, y0, z0=NULL) {
  if (nrow(coords)>1) {
    printf("Input dataframe coords must only have one row!")
    return(F)
  }
  coords$compassaz <- ifelse(coords$compassaz <= 90, 90 - coords$compassaz, 450 - coords$compassaz)
  x <- x0 + coords$compasshd * cos(coords$compassaz / 180 * pi)
  y <- y0+ coords$compasshd * sin(coords$compassaz / 180 * pi)
  z <- NA
  if (is.null(z0)==F & is.na(coords$compassdeg)==F) {
    zOff <- round(tan(coords$compassdeg*(pi/180))*coords$compasshd,digits = 1)
    if ((zOff==coords$compassh)==F) #printf("Compassh value offsets by %g. Adjusted height accordingly\n",zOff-coords$compassh)
    z <- z0+zOff
  }
  return(data.frame(x=x,y=y,z=z))
}

#get distance matrix for mappedobjects with xyz data
get.xyz.dist <- function(mo,plot,namesCol) {
  if (F%in%c("x","y","z")%in%names(mo)) {
    printf("Input dataframe coords must contain x,y,z information!")
    return(F)
  }
  if (nrow(mo)<2) {
    printf("Input dataframe coords must have more than one row!")
    return(F)
  }
  coords <- mo[which(as.numeric(mo[,which(grepl(pattern = "plot",x = names(mo),ignore.case = T))])==as.numeric(plot)),]
  d <- dist.xyz(a = as.matrix(coords[,c("x","y","z")]))
  colnames(d) <- coords[,namesCol]
  rownames(d) <- coords[,namesCol]
  return(d)
}

#get covariance structure from distance matrix
get.xyz.K <- function(mo,plot,namesCol) {
  d <- get.xyz.dist(mo,plot,namesCol)
  d <- d/max(as.numeric(d))
  d <- 1-d
  return(d)
}

cleanMappoints <- function(allMO) {
  cleanMO <- allMO[0, ]
  for (mp in unique(allMO$mappointid)) {
    currPlot <- allMO$plotid[which(allMO$mappointid==mp)[1]]
    mo <- allMO[which(allMO$mappointid==mp),]
    mo <- mo[which(duplicated(mo[,which(colnames(mo)%in%c("mappedobjectid","comment","sampletime")==F)])==F),]
    if ("ignore"%in%names(mo)) mo <- mo[which(mo$ignore==F),]
    #dups <- potentialDups(mo)
    if (nrow(mo)<1) next
    if (F%in%is.na(mo$compasshd)==F) next
    cleanMO <- rbind(cleanMO,mo)
  }
  cleanMO$treeid <- as.numeric(apply(MARGIN = 1,X = as.matrix(cleanMO$treeid),FUN = substring,first=6,last=8))
  if (nrow(cleanMO)<1) return(NULL)
  return(cleanMO)
}

plotEachMappoint <- function(cleanmo,outdir,datasetName) {
  for (mp in unique(cleanmo$mappointid)) {
    currPlot <- cleanmo$plotid[which(cleanmo$mappointid==mp)[1]]
    mo <- cleanmo[which(cleanmo$mappointid==mp),]
    mo <- mo[which(duplicated(mo[,which(colnames(mo)%in%c("mappedobjectid","comment","sampletime")==F)])==F),]
    if ("ignore"%in%names(mo)) mo <- mo[which(mo$ignore==F),]
    if (nrow(mo)<1) next
    if (F%in%is.na(mo$compasshd)==F) next
    if (dir.exists(paste(outdir,"/",datasetName,"_PlotMaps",sep=""))==F) dir.create(paste(outdir,"/",datasetName,"_PlotMaps",sep=""))
    pdf(file = paste(outdir,"/",datasetName,"_PlotMaps/",currPlot,"_MP",mp,"_PlotMap2D.pdf",sep=""),width = 7,height = 7,useDingbats = F)
    MapForMapPoint(mo,mp)
    dev.off()
  }
  return(cleanMO)
}

#' Generate and plot MDS (metric) using cmdscale [stats]
#' @param dist this can be either a square dataframe/matrix representing a distance matrix generated elsewhere or a filename pointing to a TASSEL generated IBS distance matrix
#' @param info either a tab delimited file with headers where the samples must be in a column called "Taxon" that matches the rownames of the distance matrix or a data.frame with the same properties
#' @param group header from infoFile will be used for grouping data. Will automatically look for a column called "'groupCol'_color" or will autogenerate colors
#' @param k optional number of coordinates to model
#' @param main optional title for plot
#' @param ptSize optional, will output plots to pdf
#' @param pdfFile optional, will output plots to pdf
#' @return MDS fit object
#' @export
MDS <- function(dist,info,group,groupbg,heat=F,k=2,main=NULL,ptSize=.6,pdfFile=NULL,cex.main = 1, cex.lab = 1, cex.axis = 1,mar= c(5, 4, 4, 2) + 0.1) {
  if (is.character(dist) && file.exists(dist)) {
    #distances <- read.table(dist,sep="\t",header=F,skip=5,row.names=1,as.is=T) #hamming (IBS) distance matrix where only rownames are labelled (TASSEL format)
    distances <- as.data.frame(fread(dist,sep="\t",skip=5,stringsAsFactors = F)) #hamming (IBS) distance matrix where only rownames are labelled (TASSEL format)
    rownames(distances) <- distances$V1
    distances <- distances[,2:ncol(distances)]
    colnames(distances) <- rownames(distances)
    if (nrow(distances)!=ncol(distances)) printf("Cannot correctly read in %s! Make sure it's a valid tassel IBS distance matrix",dist)
  } else if (nrow(dist)==ncol(dist)) {distances <- dist
  } else {
    printf("\nNon-valid entry for dist variable! Must be either a tassel IBS distance matrix file or a square data.frame or matrix")
    return(NULL)
  }
  if (is.character(info) && file.exists(info)) {col <- read.table(info,header=T,sep="\t",as.is=T)
  } else {col <- info}
  if ("taxa"%in%tolower(names(col))==T) {
    names(col)[which(tolower(names(col))=="taxa")] <- "Taxon"
  }
  if ("taxon"%in%tolower(names(col))==F) {
    printf("\nProblem with info! Make sure there is a column in info called Taxon!")
    return(NULL)
  }
  taxonCol <- which("taxon"==tolower(names(col)))
  if (group%in%names(col)==F) {
    printf("\nProblem with info! Your group variable is not a viable header")
    return(NULL)
  }
  if (nrow(col)<1) {
    printf("\nNo rows in info file!")
    return(NULL)
  }
  #str(col)
  if (heat ==F) col <- col[which(col[,paste(group,"color",sep="_")]!=''),]
  else col <- col[which(col[,group]!='' | is.na(col[,group])==F),]
  distances <- distances[which(rownames(distances)%in%col[,taxonCol]==T),which(rownames(distances)%in%col[,taxonCol]==T)]
  if (nrow(distances)<1) {
    printf("\nNo rows left in distances after filtering based on infofile!")
    return(NULL)
  }
  col <- col[which(col[,taxonCol]%in%rownames(distances)),]
  rem.na <- apply(distances, 2, function(x) length(which(is.na(x)==T)))
  keep <- which(rem.na==0 | rem.na<max(rem.na)) #
  while (length(keep)<nrow(distances)) {
    if ((nrow(distances)-length(keep))>0) {
      printf("\nRemoving samples from distance matrix driving NA values: %s",paste(rownames(distances)[which(1:nrow(distances)%in%keep==F)],collapse=","))
      distances <- distances[keep,keep]
    }
    rem.na <- apply(distances, 2, function(x) length(which(is.na(x)==T)))
    keep <- which(rem.na==0 | rem.na<max(rem.na)) #
  }
  col <- col[which(col[,taxonCol]%in%rownames(distances)),]
  d <- as.matrix(distances)
  fit <- cmdscale(d,k=k,eig=T) # k is the number of dim
  #fit # view results
  if (heat==T) {
    heatmap <- getHeatMapCol(col[,group],color.range = c("blue","yellow","red"))
    color.acc <- heatmap$cols[match(rownames(fit$points[]),col[,taxonCol])]
  }else {
    legend <- levels(factor(col[,group][match(rownames(fit$points[]),col[,taxonCol],)],levels = unique(col[,group])))
    #legend
    if (paste(group,"color",sep="_")%in%names(col)==F) {
      color.legend <- uniqueCols(length(legend))
      color.acc <- color.legend[match(col[,group],legend)]
    } else {
      color.legend <- col[,paste(group,"color",sep="_")][match(legend,col[,group])]
      color.acc <- col[,paste(group,"color",sep="_")][match(rownames(fit$points[]),col[,taxonCol])]
    }
    }# plot solution
  if (is.null(pdfFile)==F) svg(pdfFile,width = 6,height = 5)
  for (i in 1:(k-1)) {
    par(mar=mar)
    j <- i+1
    x <- fit$points[,i]
    y <- fit$points[,j]
    if (is.null(main)) main <- paste("Metric MDS by",group)
    plot(x, y, xlab=paste("Coordinate ",i," (",round((fit$eig[i]/sum(fit$eig))*100,1),"%)",sep=""), ylab=paste("Coordinate ",j," (",round((fit$eig[j]/sum(fit$eig))*100,1),"%)",sep=""),main=main,col=color.acc,bg=color.acc,pch=21,lwd=2,cex=ptSize,xlim=c(min(fit$points[,1]),max(fit$points[,1])+(max(fit$points[,1])-min(fit$points[,1]))*.3),cex.main=cex.main,cex.lab=cex.lab,cex.axis=cex.axis)
    if (heat==T) {
      xrange <- (par("usr")[2]-par("usr")[1])
      yrange <- (par("usr")[4]-par("usr")[3])
      addColorScale(info = col,heatmap = heatmap,colorTerm = group,xl <- c(par("usr")[2]-xrange/5,par("usr")[2]-xrange/6),yl <- c(par("usr")[3]+yrange/4,par("usr")[4]-yrange/4))
    }else {legend(x = "bottomright",legend = legend,col=color.legend,pch=19,cex=0.6)}
  }
  if (is.null(pdfFile)==F) dev.off()
  return(list(fit=fit,info=col,d=distances))
}

#Colors
#' get heatmap colors for a vector of numbers. Breaks generated by rounding, so if very small numbers, breaks can be increased by multiplying by a scalar
#' @param mp the vector of numbers
#' @param color.range the range of colors to generate the palette from
#' @return list where cols are the colors corresponding to the input vectors and pal is the colorRampPalette object
#' @export
getHeatMapCol <- function(mp,color.range=c("blue","green","yellow","orange","red")) {
  breaks <- round(mp,0)
  if (all(is.na(breaks))) return(NULL)
  pal <- colorRampPalette(bias = 1,alpha=T,colors = color.range,interpolate = "linear")(max(breaks,na.rm = T)-min(breaks,na.rm = T)+1)
  cols <- data.frame(breaks=seq(min(breaks,na.rm = T),max(breaks,na.rm = T),1),col=pal,stringsAsFactors = F)
  return(list(cols=cols$col[match(breaks,cols$breaks)],pal=pal))
}

#' get N unique colors from the brewer palatte (if less than 72) or from rainbow if more
#' @param N number of unique colors
#' @param opacity in hex notation (https://gist.github.com/lopspower/03fb1cc0ac9f32ef38f4). Defaults to 50%
#' @return vector of unique hex colors
#' @export
uniqueCols <- function(N,opacity="80") {
  if (N<75) {
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_max = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    return(paste(sample(col_max, N),opacity,sep=""))
  } else {
    return(paste(substring(text = rainbow(n = N,alpha = 1),1,7),opacity,sep=""))
  }
}

# Function to plot color bar
color.bar <- function(lut, min,max=-min, nticks=3, ticks=seq(min, max, len=nticks), title='',sideaxt=4) {
  scale = (length(lut)-1)/(max-min)
  #dev.new(width=1.75, height=5)
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main='')
  title(main = title,adj=0,cex.main=.8,line = .5,xpd=T)
  axis(sideaxt, round(ticks,2), las=1,line = -.9,tick = F,cex.axis=.8,xpd=T)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}

addColorScale <- function(info,heatmap,colorTerm,xl=NULL,yl=NULL,sideaxt=4,title=NULL) {
  xrange <- (par("usr")[2]-par("usr")[1])
  yrange <- (par("usr")[4]-par("usr")[3])
  if (is.null(xl)) xl <- c(par("usr")[2]-xrange/10,par("usr")[2]-xrange/20)
  if (is.null(yl)) yl <- c(par("usr")[3]+yrange/4,par("usr")[4]-yrange/4)
  if (is.null(title)) title <- colorTerm
  Hmisc::subplot(fun = color.bar(lut = heatmap$pal,min = min(info[,colorTerm],na.rm = T),title=title,max = max(info[,colorTerm],na.rm = T),sideaxt = sideaxt),hadj = 1,vadj = 0,x = xl,y = yl)
}

#Setup plotting devices for different types of plots
SetUpCircularPlot <- function() {
  # Set up plotting device
  plot.new()
  par(mar=c(1, 1, 1, 2), oma=rep(0,4))
  plot.window(xlim=c(-30, 30), ylim=c(-30, 30), asp=1)
  xrange <- par("usr")[2]-par("usr")[1]
  yrange <- par("usr")[4]-par("usr")[3]
  text(x = c(-29+xrange/100),y = c(-30+yrange/100),labels=c("2m"),cex=1)
  lines(x=c(-30+xrange/100,-30+xrange/100+2),y=c(-27, -27),lwd=2)
  lines(x=c(-30+xrange/100,-30+xrange/100),y=c(-27-yrange/200, -27+yrange/200),lwd=2)
  lines(x=c(-30+xrange/100+2,-30+xrange/100+2),y=c(-27-yrange/200, -27+yrange/200),lwd=2)
  
  lines(get.coords(seq(0, 450, length.out=1000), 11, 0, 0),lty = 2,col="grey")
  lines(get.coords(seq(0, 450, length.out=1000), 21, 0, 0),lty = 1,col="black")
  lines(get.coords(seq(0, 450, length.out=1000), 26, 0, 0),lty = 2,col="red")
  
  # 45-degree lines
  apply(get.coords(seq(15,360, 15), 28, 0, 0), 1, 
        function(x) lines(rbind(x, c(0, 0)), lwd=2))
  
  # Plot white curves over black curves and add text
  sapply(c(11,21,26), function(x) {
    txt <- paste0(x, 'm')
    w <- strwidth(txt, cex=0.9)/2
    a <- atan(w/x)/pi*180
    lines(get.coords(seq(-a, a, length=30), x, 0, 0),lwd=2.5, col='white')
    lines(x = c(0,0),y = c(x-1,x+1),lwd=2.5, col='white')
    text(0, x, txt, cex=0.8)
  })
  text(x = c(0,30,0,-30),y = c(30,0,-30,0),labels=c("N","E","S","W"),cex=1.8)
  return(TRUE)
}
SetUpSquarePlot <- function(mo,mpr,plot) {
  mprcurr <- mpr[which(mpr$plotid==plot),]
  if (c("x")%in%names(mo)==F) {
    printf("Might be more efficient to run consensusCoordinates before MapPoints! Running now\n")
    map <- consensusCoordinates(mo)
  }
  # Set up plotting device
  plot.new()
  par(mar=c(2, 0, 0, 0), oma=rep(0, 4))
  plot.window(xlim=c(min(mo$x)-4,max(mo$x)+1), ylim=c(min(mo$y)-1,max(mo$y)+4), asp=1)
  text(0,0,unique(mprcurr$centerpoint))
  for (p in which(mprcurr$mappointid==unique(mprcurr$centerpoint))) {
    text(get.coords(a = mprcurr[p,"compassaz"],d=mprcurr[p,"compasshd"],0,0),labels = mprcurr[p,"mappointtarget"])
  }
  text(x = c(0),y = c(max(mo$y)+2),labels=c("N"),cex=1.8)
  text(x = c(min(mo$x)-4),y = c(max(mo$y)+4),pos = 4,labels = paste("Plot ID: ",plot,sep=""),cex=1)
  text(x = c(min(mo$x)-4),y = c(min(mo$y)-1),labels=c("2m"),cex=1)
  lines(x=c(min(mo$x),min(mo$x)+2),y=c(min(mo$y)-1,min(mo$y)-1),lwd=2)
  return(TRUE)
}

MapPoints <- function(map,fillcol="#00000000",outlinecol="#00000000",lwd=2,txt=NULL) {
  if (nrow(map)<1) return(NULL)
  if (c("x")%in%names(map)==F) {
    printf("Must run consensusCoordinates before MapPoints! Might be more efficient to do it once. Running now\n")
    map <- consensusCoordinates(map)
  }
  dbh <- log(1+map$dbhcm/10)
  points(x = map$x,y = map$y,pch=21,col=outlinecol,bg = fillcol,lwd=lwd,cex=dbh)
  if (is.null(txt)==F & length(txt)>0) text(x = map$x,y = map$y,labels = txt,pch=21,col=fillcol,adj = c(0,2),cex=.5)
  return(map)
}

#Expects inputs based on haglof data in trgdb
getMPRelations <- function(mo) {
  #if (length(unique(mo$plotid))<1) return(NULL)
  query <-  paste("SELECT p.centerpoint, p.ne, p.se, p.nw, p.sw, m.*, mp.transponderheightcm FROM plot p INNER JOIN mappedobject m ON p.plotid = m.plotid  INNER JOIN mappoint mp on m.mappointid = mp.mappointid where m.plotid in (",paste(unique(mo$plotid),collapse = ", "),") and m.mappointtarget IS NOT NULL ",sep="")
  mpr <- getDataFromDB(query = query)
  return(mpr)
}
consensusCoordinates <- function(mo) {
  mpr <- getMPRelations(mo)
  mo$x <- numeric(nrow(mo))
  mo$y <- numeric(nrow(mo))
  mo$z <- numeric(nrow(mo))
  if (is.null(mpr) || nrow(mpr)<1) {
    printf("No alternate mapping points included. Assume all mappings coming from 0,0,0\n")
    for (i in 1:nrow(mo)) {
      mo[i,c("x","y","z")] <- get.coords.TRG(coords = mo[i,],x0 = 0,y0 = 0,z0 = 0)
    }
    return(list(mo=mo,mpr=mpr))
  }
  for (plot in unique(mpr$plotid)) {
    mcurr <- mpr[which(mpr$plotid==plot),]
    center <- unique(mcurr$centerpoint)
    for (i in which(mo$mappointid==center)) {
      mo[i,c("x","y","z")] <- get.coords.TRG(coords = mo[i,],x0 = 0,y0 = 0,z0 = 0)
    }
    for (altp in mcurr[1,2:5]) {
      curralt <- mcurr[which(mcurr$mappointtarget%in%altp==T),]
      if (nrow(curralt)>1) {
        printf("More than one measurement from center for centerpoint %f and alternate point %f",center,altp)
        return(F)
      }
      if (nrow(curralt)>0) {
        st <- get.coords.TRG(curralt,0,0,0)
        for (i in which(mo$mappointid==altp)) {
          mo[i,c("x","y","z")] <- get.coords.TRG(coords = mo[i,],x0 = st$x,y0 = st$y,z0 = st$z)
        }
      }
    }
  }
  return(list(mo=mo,mpr=mpr))
}
uniqueCols <- function(N,opacity="80") {
  if (N<75) {
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_max = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    return(paste(sample(col_max, N),opacity,sep=""))
  } else {
    return(paste(substring(text = rainbow(n = N,alpha = 1),1,7),opacity,sep=""))
  }
}
potentialDupsByDist <- function(mo,plot,square=F,mpr=F,quantile=1,main=NULL) {
  if (c("x")%in%names(mo)==F) {
    printf("Must run consensusCoordinates before MapPoints! Might be more efficient to do it once. Running now\n")
    mo <- consensusCoordinates(mo)
  }
  mpVals <- unique(mo$mappointid)[order(unique(mo$mappointid))]
  mpCols <- c("#b8477a","#778d2a","#a05740","#626c8e","#285225","#793ea1","#d3582d","#8eb4ff","#61544e","#e086ff")[1:length(mpVals)]
  matchint <- match(mo$mappointid,table = mpVals)
  mo$outlinecol <- mpCols[matchint]
  
  m <- as.matrix(mo[,c("x","y","z")])
  rownames(m) <- mo$mappedobjectid
  mnorm <- scale(m, center=T, scale=colSums(m))
  d <- dist(m,method = "euclidean")
  distances <- as.matrix(d)
  diag(distances) <- NA
  rownames(distances) <- labels(d)
  colnames(distances) <- labels(d)
  quants <- quantile(x = d,na.rm = T,probs = c(quantile, NA)/100)
  sub <- unique(d[which((d<quants[paste(quantile,"%",sep="")])==T)])
  #unique cols
  cols <- uniqueCols(length(sub))
  smo <- mo[0,c("mappedobjectid","plotid","mappointid","Combined","status","dbhcm","x","y","z","outlinecol")]
  smo$dist <- numeric(0)
  smo$col <- character(0)
  for (s in sub) {
    pr <- rownames(which(distances==s, arr.ind = TRUE))
    cur <- mo[which(mo$mappedobjectid%in%pr),]
    cur$dist <- rep(s,nrow(cur))
    if (is.na(sd(cur$dbhcm)) | sd(cur$dbhcm)<(min(cur$dbhcm)*.2)) {
      add <- cbind(cur[,c("mappedobjectid","plotid","mappointid","Combined","status","dbhcm","x","y","z","outlinecol")],dist=s,col=cols[which(sub==s)])
      matchR <- which(do.call(paste0, smo[,-which(names(smo)%in%c("col"))]) %in% do.call(paste0, add[,-which(names(add)%in%c("col"))])==T)
      if (length(matchR)==0 || diff(matchR)!=1) {
        smo <- rbind(smo,add)
      }
    }
  }
  smo$col <- as.character(levels(smo$col))[smo$col]
  if (square==F) {
    SetUpCircularPlot()
  } else {SetUpSquarePlot(mo,mpr,whichPlot)}
  text(x = c(min(mo$x)-4),y = c(max(mo$y)+2),pos = 4,labels = paste("Potential Dups",main,sep="\n"),cex=1)
  uniq <- which(do.call(paste0, mo[,names(smo)[-which(names(smo)%in%c("dist","col"))]]) %in% do.call(paste0, smo[,-which(names(smo)%in%c("dist","col"))])==T)
  MapPoints(mo[-which(1:nrow(mo)%in%uniq),],fillcol="#0000000D",outlinecol = mo[-which(1:nrow(mo)%in%uniq),"outlinecol"],lwd=2)
  MapPoints(smo,fillcol=smo$col,lwd=2,outlinecol = smo$outlinecol,txt=smo$Combined)
  return(smo)
}
subsetForMP <- function(mo,whichMP) {
  return(mo[which(mo$mappointid%in%whichMP),])
}
subsetForPlot <- function(mo,whichPlot) {
  return(mo[which(mo$plotid%in%whichPlot),])
}

checkUniqueNamedTrees <- function(mo) {
  named <- mo$treeid[which(is.na(mo$treeid)==F)][order(as.numeric(mo$treeid[which(is.na(mo$treeid)==F)]))]
  dups <- named[which(duplicated(named)==T)]
  uniq <- unique(named)[which(unique(as.numeric(named))%in%(1:max(as.numeric(named)))==F)]
  if (length(unique(named))==length(1:max(as.numeric(named)))) {
    print(paste(max(as.numeric(named)),"trees unique and accounted for!"))
  } else if (length(dups)>0) {
    print(paste("duplicated trees:",paste(dups,collapse = ", ")))
  } else if (length(uniq)>0) {
    print(paste("missing trees:",paste(uniq,collapse = ", ")))
  }
}


convertToKML <- function(df = NULL,csvFile = NULL,outDir=NULL,outBase="out",idCol=NULL,keepCol=all,crsString="+init=epsg:4326") {
  if (is.null(df) & is.null(csvFile)==F) df <- read.csv(csvFile)
  colnames(df) <- tolower(colnames(df))
  if (all(c("latitude","longitude")%in%colnames(df))) {coordNames <- c("latitude","longitude")
  } else if (all(c("lat","long")%in%colnames(df))) {coordNames <- c("lat","long")
  } else if (all(c("x", "y")%in%colnames(df))) {coordNames <- c("x", "y") 
  } else {
    print("Couldn't find headers entitled latitude/longitude, lat/long or x/y!")
    return(NULL)
  }
  if (is.null(idCol)) idCol <- colnames(df)[1]
  if (keepCol[1]=="all") {
    keepCol <- colnames(df)
  } else {
    keepCol <- tolower(keepCol)
    keepCol <- keepCol[which(keepCol%in%colnames(df))]
  }
  coordinates(df) <- coordNames[c(2,1)]
  proj4string(df) <- CRS(crsString)
  df_ll <- spTransform(df, CRS("+proj=longlat +datum=WGS84"))
  if (is.null(csvFile)==F & outBase=="out") outBase <- strsplit(x = basename(csvFile),split = ".",fixed = T)[[1]][1]
  if (is.null(outDir) & is.null(csvFile)==F) outDir <- dirname(csvFile)
  dir.create(file.path(outDir, outBase))
  out <- paste(outDir,"/",outBase,".kml",sep="")
  write.table('<?xml version="1.0" encoding="UTF-8"?>',out,quote = F,col.names = F,row.names = F)
  write.table('<kml xmlns="http://www.opengis.net/kml/2.2">',out,quote = F,col.names = F,row.names = F,append=T)
  write.table(paste('<Document id="',outBase,'">',sep=""),out,quote = F,col.names = F,row.names = F,append=T)
  for (i in seq(1,nrow(df_ll),1)) {
    write.table('\t<Placemark>',out,quote = F,col.names = F,row.names = F,append=T)
    write.table(paste('\t\t<name>',df_ll@data[i,which(names(df_ll)==idCol)],'</name>',sep=""),out,quote = F,col.names = F,row.names = F,append=T)
    write.table(paste('\t\t<description>',paste(paste(keepCol,df_ll@data[i,keepCol],sep="="),collapse=", "),'</description>',sep=""),out,quote = F,col.names = F,row.names = F,append=T)
    write.table('\t\t<Point>',out,quote = F,col.names = F,row.names = F,append=T)
    write.table(paste('\t\t\t<coordinates>',paste(df_ll@coords[i,c("x","y")],collapse = ","),'</coordinates>',sep=""),out,quote = F,col.names = F,row.names = F,append=T)
    write.table('\t\t</Point>',out,quote = F,col.names = F,row.names = F,append=T)
    write.table('\t</Placemark>',out,quote = F,col.names = F,row.names = F,append=T)
  }
  write.table('</Document>',out,quote = F,col.names = F,row.names = F,append=T)
  write.table('</kml>',out,quote = F,col.names = F,row.names = F,append=T)
  return(df_ll)
}



