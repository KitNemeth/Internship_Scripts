#Admixture
library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
file <- "MasterLandraceTeoInbredGBS_collapseDist0.02_collapseAL_subset_minTaxa01_admixfilt_rmvCloseKin0.03_poly_minSiteCov0.5_minTaxaCov0.3_RmvHighLD_rmvThird" #"ZeaGBSv27_20150108AGPv3MatchSites_subsetBy12S_13S_RIMMA_JGSTeo_NFlint_TP_ALLsingleRep_HWEByDepth9_minTaxaCov.3_rmvThird_poly_minSiteCov.5_removeCloseKin0.15." #ZeaGBSv27_20150108AGPv3.hmp_subBy12S_13S_RIMMA_NAMHighCov_MR_TIL_Goldstein_TP15_JGSTeoHWEByDepth9_minTaxaCov.3_rmvThird_poly_minSiteCov.5_removeCloseKin.
order <- fread("TaxaOrder_AL_subset.txt", header=T)
col= c("#59398d",
       "#6d83da",
       "#cf9d3a",
       "#b54673",
       "#45c097",
       "#ba4c46",
       "#968a3d",
       "#bb6130")
k <- 8

#45c097

tbl=read.table(paste(file,".",k,".Q",sep = ""))

match <- match(x = read.table(paste(file,"fam",sep="."),header=F,as.is=T)[,2],table = order$OrigName)
order <- order[match,]

tbl <- cbind(tbl,order)
tbl <- tbl[order(tbl$OrigOrder),]
str(tbl)

#Change to whatever variable you sort by
tbl <- tbl[-c(which(is.na(tbl$Elevation)==T)),]
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

out="AdmixALsubset.svg"
#svg(out)
svg(out,width=20,height=10)
par(mfrow = c(3, 1),     # 2x1 layout
      oma = c(4, 2, 2, 0), # two rows of text at the outer left and bottom margin
      mar = c(0, 3, 0, 10), # space for one row of text at ticks and to separate plots
      mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
      xpd = TRUE)            # allow content to protrude into outer margin (and beyond)

#par(mfrow=c(2,1))
#par(mar=c(5.1, 4.1, 4.1, 2.1))
#Elevation plot
elev <- cbind(tbl$Elevation/max(tbl$Elevation[is.na(tbl$Elevation)==F]),1-(tbl$Elevation/max(tbl$Elevation[is.na(tbl$Elevation)==F])))
barplot(t(as.matrix(tbl$Elevation)),bty = "l", col=c("black","white"),yaxt="n", xaxt="n", border=NA,xlim=c(0,nrow(tbl)),width = 1,space = spaces,names.arg=rep("",nrow(tbl)),)
axis(2, line=-4.5,seq(0,5000,1000),font=1)
title(ylab="Elevation (m)", line=-1)
segments(x0=0,y0=1000,x1=1186,y1=1000,col="Black",lwd = 0.5, lty = "dotted")
segments(x0=0,y0=2000,x1=1186,y1=2000,col="Black",lwd = 0.5, lty = "dotted")
segments(x0=0,y0=3000,x1=1186,y1=3000,col="Black",lwd = 0.5, lty = "dotted")

#segments(x0=0,y0=0,x1=0,y1=4000,col="Black")
segments(x0=0,y0=0,x1=1186,y1=0,col="Black")
#axis(1,at = breaks[1:(length(breaks)-1)]+((0:(length(breaks)-2))*2)+((breaks[2:length(breaks)]-breaks[1:(length(breaks)-1)])/2),lwd=0,lwd.ticks=1,labels = gsub("_"," ",tbl$Source[breaks]),cex.axis= .5, las=2)
#Admixture plot
barplot(t(as.matrix(tbl[1:k])), col=col[1:k], yaxt="n", xaxt="n",border=NA,xlim=c(0,nrow(tbl)),width = 1,space = spaces,names.arg=rep("",nrow(tbl)))
axis(1,line=-0.2,at = breaks[1:(length(breaks)-1)]+((0:(length(breaks)-2))*2)+((breaks[2:length(breaks)]-breaks[1:(length(breaks)-1)])/2),lwd=0,lwd.ticks=1,labels = gsub("_"," ",tbl$Source[breaks]), las=2)
axis(2, line=-4.5,seq(0.25,0.75,0.25),font=1)
#segments(x0=0,y0=0,x1=0,y1=1,col="Black")
segments(x0=0,y0=0,x1=1186,y1=0,col="Black")
title(ylab="Ancestry", line=-1)
dev.off()

out="AdmixCVFSTALsubset.svg"
#svg(out)
svg(out,width=20,height=10)
line = 1
cex = 1
side = 3
adj=-0.1
par(mfrow=c(1,2))
#plot the CVs
#par(default.par)
cv <- read.table(paste(file,".CVlog",sep=""),as.is=T,sep = ":",header=F)
cv$K <- as.numeric(gsub(pattern = ")",replacement = "",x = gsub(pattern = "CV error (K=",replacement = "",x = cv$V1,fixed = T),fixed = T))
#cv <- c(0.7601,0.72666,0.70061,0.69188,0.68514,0.67704,0.67586,0.6725,0.67036,0.66882,0.66944,0.66957,0.67071,0.67229,0.67186)
#cv <- c(0.76528,0.72507,0.70422,0.68885,0.68528,0.68207,0.67936,0.68012,0.68167,0.68476,0.68918,0.68961,0.69282,0.6979,0.70267)
cv<-cv[order(as.numeric(cv$K)),]
#pdf(file = paste(file,"_rmvThird.CV.pdf",sep=""),width=4,height=4,useDingbats = F)
plot(x=cv$K,y=cv$V2,type = "o", main="5-fold cross validated error",xlab="K",ylab="Prediction error",pch=19)
abline(v=9, col="red", lty=2, lwd= 1.5)
#mtext("c",font = 2, side=side, line=line, cex=cex, adj=adj)


#Plot FSTS
#pdf(paste(file,".admixture.k",k,".CV_Fst.pdf",sep=""),width=k-2,height=k-2)
s <- scan(file = paste(file,".Keq",k,".CV",sep = ""),what = "complex",sep = "\n",quiet = T)
fst <- matrix(data = 0.0,nrow = k,ncol = k)
start <- which(s=="Fst divergences between estimated populations: ")+2
for (line in 1:k) {
  c <- strsplit(s[(start+line)],"\t")[[1]]
  fst[line+1,1:(length(c)-1)] <- as.numeric(c[2:length(c)])
}
fst[,k] <- as.numeric(rep(0.0,k))
par(mar=c(.1,.1,.1,.1))
plot(1:(k+3),1:(k+3),col=col[1:k],pch=15,cex=.0001,frame.plot=F,main="Modelled FST K=9",line=-2.3,xaxt="n",yaxt="n",xpd=T,ylab="",xlab="")
points(3:(k+1),rep(2,k-1),col=col[1:k-1],pch=15,cex=5,xpd=T)
points(rep(2,k-1),(k+1):3,col=col[2:k],pch=15,cex=5,xpd=T)
for (r in 1:nrow(fst)) {
  for (c in 1:ncol(fst)) {
    if (fst[k+1-r,c]!=0) {text(c+2,r+2,fst[k+1-r,c],font = 2)}
  }
}
#mtext("d",font = 2, side=side, line=1, cex=cex, adj=adj)

dev.off()
