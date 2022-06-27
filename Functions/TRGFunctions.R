#####These are functions for interacting with data from the Swarts Lab, especially designed to connect to the lab azure database (trg-srv.postgres.database.azure.com)
###Kelly Swarts, 2019

Sys.setenv(lang="EN")
install.packages("Hmisc")
install.packages("RColorBrewer")
install.packages("bio3d")
install.packages("jsonlite")
install.packages("R.utils")
install.packages("stringr")
install.packages("rworldmap")
install.packages("elevatr")

library("Hmisc")
library("RColorBrewer")
library("bio3d")
library("jsonlite")
library("R.utils")
library("stringr")
library("rworldmap")
library("elevatr")

options(digits=6)

###Global variables for mapping 
col.status <- c("#000000","#2F4F4F","#778899","#A9A9A9","#DCDCDC")
names(col.status) <- c("Live","Standing Dead","Fallen Dead","Uprooted","Stump")
col.species <- c("#5ea45e","#c09b1f","#F1E2CC","#A65628","#FF7F00","#E7298A","#FDDAEC","#BEBADA","#7FC97F","#FDBF6F","#CCCCCC","#FFFFB3","#377EB8","#DECBE4","#E5D8BD","#FFFF99","#666666","#D9D9D9","#B15928","#FFF2AE","#FFFFCC","#CCEBC5","#66A61E","#B3DE69","#B3E2CD","#7570B3","#E31A1C","#33A02C","#E41A1C","#B2DF8A","#F0027F","#1F78B4","#FFFF99","#BC80BD","#CBD5E8","#F781BF","#FB8072","#FC8D62","#FFD92F","#984EA3","#FFFF33","#80B1D3","#FDC086","#B3CDE3","#6A3D9A","#FFED6F","#BEAED4","#F4CAE4","#999999","#D95F02","#386CB0","#E6AB02","#E78AC3","#8DD3C7","#E6F5C9","#FDB462","#A6D854","#E5C494","#A6761D","#CAB2D6","#FF7F00","#A6CEE3","#CCEBC5","#FB9A99","#66C2A5","#FED9A6","#FCCDE5","#FBB4AE","#F2F2F2","#666666","#B3B3B3","#4DAF4A","#BF5B17")
names(col.species) <- seq(1,33,1)
names.species <- c("Picea abies","Pinus sylvestris","Fagus sylvatica","Carpinus betulus","Fraxinus spp.","Ilex aquifolium","Larix spp.","Populus spp.","Betula spp.","Quercus spp.","Pseudotsuga menziesii","Abies spp.","Juniperus spp.","Cupressus spp.","Arabidopsis thaliana","Zea mays mays","Zea mays mexicana","Zea mays parviglumis","Acer psudoplatanus","Sorbus acuparia","Petasites spp","Athyrium filix-femina","Abies alba","Acer spp.","UNK spp.","Ips typographus","Abies concolor","Pinus ponderosa","Gossypium spp.","Cedrus spp.","Cannabis sativa","Salix spp.","Pinus mugo")


####Columns for different db tables
plotCols <- c("plotid","collected","slope","aspect","altitude","centerpoint","ne","se","nw","sw","country","recorder","person_soilmoisture","person_dna","photolocation","comment","altname")

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

get.coords <- function(a, d, x0, y0) {
  a <- ifelse(a <= 90, 90 - a, 450 - a)
  data.frame(x = x0 + d * cos(a / 180 * pi), 
             y = y0+ d * sin(a / 180 * pi))
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
  par(mar=mar)
  for (i in 1:(k-1)) {
    j <- i+1
    x <- fit$points[,i]
    y <- fit$points[,j]
    if (is.null(main)) main <- paste("Metric MDS by",group)
    plot(x, y, xlab=paste("Coordinate ",i," (",round((fit$eig[i]/sum(fit$eig))*100,1),"%)",sep=""), ylab=paste("Coordinate ",j," (",round((fit$eig[j]/sum(fit$eig))*100,1),"%)",sep=""),main=main,col=color.acc,bg=color.acc,pch=21,lwd=2,cex=ptSize,xlim=c(min(fit$points[,1]),max(fit$points[,1])+(max(fit$points[,1])-min(fit$points[,1]))*.3),cex.main=cex.main,cex.lab=cex.lab,cex.axis=cex.axis)
    if (heat==T) {
      xrange <- (par("usr")[2]-par("usr")[1])
      yrange <- (par("usr")[4]-par("usr")[3])
      addColorScale(info = col,heatmap = heatmap,colorTerm = group,xl <- c(par("usr")[2]-xrange/5,par("usr")[2]-xrange/6),yl <- c(par("usr")[3]+yrange/4,par("usr")[4]-yrange/4))
    }else {legend(x = "bottomright",legend = legend,col=color.legend,pch=19,cex=0.75)}
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


uniqueCols <- function(N,opacity="80") {
  if (N<75) {
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_max = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    return(paste(sample(col_max, N),opacity,sep=""))
  } else {
    return(paste(substring(text = rainbow(n = N,alpha = 1),1,7),opacity,sep=""))
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



