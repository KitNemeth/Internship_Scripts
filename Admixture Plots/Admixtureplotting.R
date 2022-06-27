#Admixture
library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
file <- "MasterLandraceTeoInbredGBS_collapseDist0.02_SouthAmericanLandraces_admixfilt_rmvCloseKin0.03_poly_minSiteCov0.5_minTaxaCov0.3_RmvHighLD_rmvThird" #"ZeaGBSv27_20150108AGPv3MatchSites_subsetBy12S_13S_RIMMA_JGSTeo_NFlint_TP_ALLsingleRep_HWEByDepth9_minTaxaCov.3_rmvThird_poly_minSiteCov.5_removeCloseKin0.15." #ZeaGBSv27_20150108AGPv3.hmp_subBy12S_13S_RIMMA_NAMHighCov_MR_TIL_Goldstein_TP15_JGSTeoHWEByDepth9_minTaxaCov.3_rmvThird_poly_minSiteCov.5_removeCloseKin.
order <- fread("TaxaOrder.txt", header=T)
col= c("#7e3179","#74b44f","#7174de","#bbb136","#533d92","#5dc67f","#ce63be","#3c7c3d","#dc6598","#43c29e","#912c5d","#36dee6","#d55947","#5e87d3","#c97426","#c187d4","#627421","#b9475f","#b3b45b","#ca525a","#c29540","#8a321e","#c1804b")
k <- 23

tbl=read.table(paste(file,".",k,".Q",sep = ""))

match <- match(x = read.table(paste(file,"fam",sep="."),header=F,as.is=T)[,2],table = order$OrigName)
order <- order[match,]

tbl <- cbind(tbl,order)
tbl <- tbl[order(tbl$OrigOrder),]
str(tbl)

#Change to whatever variable you sort by
tbl <- tbl[-c(which(is.na(tbl$Beck)==T)),]
tbl$Source <- factor(tbl$Source)
rownames(tbl) <- seq(from = 1,to=nrow(tbl),by=1)

breaks <- 0
for (i in 2:nrow(tbl)) {
  if (tbl$Source[i]!=tbl$Source[i-1]) {
    breaks <- c(breaks,i-1)
  }
}
breaks <- c(breaks,nrow(tbl))
#breaks <- 0
#spaces <- 0

spaces <- rep(0,nrow(tbl))
spaces[breaks[2:(length(breaks)-1)]+1] <- 2
labels <- rep("",nrow(tbl))
labels[breaks[1:(length(breaks)-1)]+((breaks[2:length(breaks)]-breaks[1:(length(breaks)-1)])/2)] <- gsub("_"," ",tbl$Source[breaks])

out="Admix1.svg"
svg(out)
svg(out,width=20,height=5)
par(mfrow = c(2, 1),     # 2x1 layout
      oma = c(4, 2, 2, 0), # two rows of text at the outer left and bottom margin
      mar = c(0, 3, 0, 0), # space for one row of text at ticks and to separate plots
      mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
      xpd = FALSE)            # allow content to protrude into outer margin (and beyond)

#par(mfrow=c(2,1))
#par(mar=c(5.1, 4.1, 4.1, 2.1))
#Elevation plot
elev <- cbind(tbl$Elevation/max(tbl$Elevation[is.na(tbl$Elevation)==F]),1-(tbl$Elevation/max(tbl$Elevation[is.na(tbl$Elevation)==F])))
barplot(t(as.matrix(tbl$Elevation)),bty = "l", col=c("black","white"), ylab="Elevation (m)",yaxt="n", xaxt="n", border=NA,xlim=c(0,nrow(tbl)),width = 1,space = spaces,names.arg=rep("",nrow(tbl)),)
axis(2, line=-1.08,seq(0,5000,1000),font=1)
#segments(x0=0,y0=0,x1=0,y1=4000,col="Black")
segments(x0=0,y0=0,x1=2240,y1=0,col="Black")
#axis(1,at = breaks[1:(length(breaks)-1)]+((0:(length(breaks)-2))*2)+((breaks[2:length(breaks)]-breaks[1:(length(breaks)-1)])/2),lwd=0,lwd.ticks=1,labels = gsub("_"," ",tbl$Source[breaks]),cex.axis= .5, las=2)
#Admixture plot
barplot(t(as.matrix(tbl[1:k])), col=col[1:k], ylab="Ancestry", yaxt="n", xaxt="n",border=NA,xlim=c(0,nrow(tbl)),width = 1,space = spaces,names.arg=rep("",nrow(tbl)))
axis(1,line=-0.1,at = breaks[1:(length(breaks)-1)]+((0:(length(breaks)-2))*2)+((breaks[2:length(breaks)]-breaks[1:(length(breaks)-1)])/2),lwd=0,lwd.ticks=1,labels = gsub("_"," ",tbl$Source[breaks]),cex.axis= .5, las=2)
axis(2, line=-1.08,seq(0.25,0.75,0.25),font=1)
#segments(x0=0,y0=0,x1=0,y1=1,col="Black")
segments(x0=0,y0=0,x1=2240,y1=0,col="Black")
dev.off()

