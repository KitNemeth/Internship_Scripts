## Admixture
# Introduction 
This script details the process of admixture analysis and subsequent plots that can bre created with the generated data.
# Data required 
You will need an .h5 file
Example = "/groups/swarts/lab/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810/MasterLandraceTeoInbredGBS_collapseDist0.02_collapseSEED_Americas-TeoSubset_minTaxa01.h5"

# Admixture filtering
This step is run in bash
Give the job a name 
```bash
#!/usr/bin/bash

#SBATCH --job-name="AdmixFiltALsubsetwithoutteo"
```
Set parameters for sbatch 
```bash
#SBATCH -N 1
#SBATCH -n 10
#SBATCH -p m
#SBATCH --mem=1000G
#SBATCH --output=AdmixFiltAL-%j.log
#SBATCH --qos=long
#taxa filtering
```
load java 
```bash
ml load java/11.0.2
```
set directory and run pipeline
```bash
dir="/groups/swarts/lab/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810";
cd $dir
tasselTRG="/groups/swarts/lab/CustomPrograms/tassel5-trg/run_pipeline.pl";
```
specify .h5 file
```bash
genos="/groups/swarts/lab/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810/MasterLandraceTeoInbredGBS_collapseDist0.02_collapseSEED_Americas-TeoSubset_minTaxa01.h5";
```
Set parameters for filtering and run
```bash
$tasselTRG -Xmx800g -FilterForAdmixturePlugin -genoFile $genos -minDistToKeep 0.03 -minSitesForHWE 30 -outFile ${genos%.h5*}"_admixfilt" &> admixtureFilter-%j.log
```
# Admixture analysis 
Next step is to run the actual analysis 
This is also run in bash 

Firstly set the job name
```bash
#!/usr/bin/bash

#SBATCH --job-name="AdmixALsubset"
```
Set sbatch parameters
```bash
#SBATCH -N 1
#SBATCH -n 10
#SBATCH -p m
#SBATCH --mem=600G
#SBATCH --output="AdmixALsubset.log"
#SBATCH --qos=long
#Admixture
```
Load the required packages
```bash 
ml load java/11.0.2
ml load plink/1.9b_6.10-x86_64
ml load build-env/2020
ml load admixture/1.3.0
```
Set the prefix for your filtered .h5 file from the previous step 
```bash
base="MasterLandraceTeoInbredGBS_collapseDist0.02_collapseSEED_Americas-TeoSubset_minTaxa01_admixfilt_rmvCloseKin0.03_poly_minSiteCov0.5_minTaxaCov0.3_RmvHighLD_rmvThird";
```
set the directroy 
```bash 
dir="/groups/swarts/lab/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810";
cd $dir
```
Make bed file for plink
```bash
plink --file "$dir/$base" --make-bed --out "$dir/$base" --noweb
```
Set the number of ks you want to test
This will find the best K value (lowest CV error) by cross validating each K
```bash
for K in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24; do
```
Now run admixture 
```bash
admixture --cv "$dir/$base.bed" $K | tee "$dir/$base.Keq${K}.CV";
admixture "$dir/$base.bed" $K;
done
grep -h CV $dir/$base.Keq*.CV > "$dir/$base.CVlog"
```
# Admixture plot
Now its time to plot your results
This step is done in R studio
Set working directory and load data.table package 
```R
library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
```
Specify the prefix for your admixture analysis results (named by Kelly not me)
```R
file <- "MasterLandraceTeoInbredGBS_collapseDist0.02_collapseSEED_Americas-TeoSubset_minTaxa01_admixfilt_rmvCloseKin0.03_poly_minSiteCov0.5_minTaxaCov0.3_RmvHighLD_rmvThird" 
```
Specify your order file which you will use to order the taxa in the plot 
Use the format shown in the table below, the ORigOrder column specifices the order of the taxa

|OrigName|	Source|	Elevation|	OrigOrder|	Latitude|	Longitude| 
|---|	---|	---|	---|	---|	---| 
|FAC-P-87_01:250039536|	Zea luxurians|	80|	1|	17.935|	-96.46833333|
Example = "/groups/swarts/lab/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810/TaxaOrder_AL.txt"
```R
order <- fread("TaxaOrder_AL.txt", header=T)
```
Specify the colours and the number of ks (determiend based on the results of cross validation)
There should be the same number of colours as ks
```R
col= c("#59398d",
       "#6d83da",
       "#cf9d3a",
       "#b54673",
       "#45c097",
       "#ba4c46",
       "#968a3d",
       "#bb6130")
k <- 8
```
The next step reads in the admixture data for the specified k and matches it to the order file
```R
tbl=read.table(paste(file,".",k,".Q",sep = ""))

match <- match(x = read.table(paste(file,"fam",sep="."),header=F,as.is=T)[,2],table = order$OrigName)
order <- order[match,]

tbl <- cbind(tbl,order)
tbl <- tbl[order(tbl$OrigOrder),]
str(tbl)

tbl$Source <- factor(tbl$Source)
rownames(tbl) <- seq(from = 1,to=nrow(tbl),by=1)
```
These steps create the bars of the admixture plot, do not change
```R
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
```
Now time to output the plot as an .svg
Specify your out file name
```R
out="AdmixALsubset.svg"
#svg(out)
```
The size of the plot can also be altered
```R
svg(out,width=20,height=10)
par(mfrow = c(3, 1),     # 2x1 layout
      oma = c(4, 2, 2, 0), # two rows of text at the outer left and bottom margin
      mar = c(0, 3, 0, 10), # space for one row of text at ticks and to separate plots
      mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
      xpd = TRUE)            # allow content to protrude into outer margin (and beyond)
```
The next step generates the elevation plot that sits atop the admixture plot
```R
elev <- cbind(tbl$Elevation/max(tbl$Elevation[is.na(tbl$Elevation)==F]),1-(tbl$Elevation/max(tbl$Elevation[is.na(tbl$Elevation)==F])))
barplot(t(as.matrix(tbl$Elevation)),bty = "l", col=c("black","white"),yaxt="n", xaxt="n", border=NA,xlim=c(0,nrow(tbl)),width = 1,space = spaces,names.arg=rep("",nrow(tbl)),)
axis(2, line=-4.5,seq(0,5000,1000),font=1)
title(ylab="Elevation (m)", line=-1)
```
Here you may need to play around with the numbers to get a line generated that is the same length as th number of taxa (X is usually slightly larger than the total number of taxa)
```R
segments(x0=0,y0=1000,x1=1076,y1=1000,col="Black",lwd = 0.5, lty = "dotted")
segments(x0=0,y0=2000,x1=1076,y1=2000,col="Black",lwd = 0.5, lty = "dotted")
segments(x0=0,y0=3000,x1=1076,y1=3000,col="Black",lwd = 0.5, lty = "dotted")
segments(x0=0,y0=0,x1=1076,y1=0,col="Black")
```
Now the admixture plot is generated 
```R
barplot(t(as.matrix(tbl[1:k])), col=col[1:k], yaxt="n", xaxt="n",border=NA,xlim=c(0,nrow(tbl)),width = 1,space = spaces,names.arg=rep("",nrow(tbl)))
axis(1,line=-0.2,at = breaks[1:(length(breaks)-1)]+((0:(length(breaks)-2))*2)+((breaks[2:length(breaks)]-breaks[1:(length(breaks)-1)])/2),lwd=0,lwd.ticks=1,labels = gsub("_"," ",tbl$Source[breaks]), las=2)
axis(2, line=-4.5,seq(0.25,0.75,0.25),font=1)
```
Again you may need to play around with the numbers to get a line generated that is the same length as th number of taxa (X is usually slightly larger than the total number of taxa)
```R
segments(x0=0,y0=0,x1=1076,y1=0,col="Black")
title(ylab="Ancestry", line=-1)
```
dev.off outputs the .svg to the specified working directory
```R
dev.off()
```
The final result should look like this:
![Picture4](https://user-images.githubusercontent.com/89838666/201969481-3ea54cb0-db46-411a-8994-7ae8eb0c2280.jpg)

Next you generate the cross-validation plot and FST plot for the ks

Again specify a name and size for the plot
```R
out="AdmixCVFSTALsubset.svg"
svg(out,width=20,height=10)
line = 1
cex = 1
side = 3
adj=-0.1
par(mfrow=c(1,2))
```
First the cross-validation values are plotted 
```R
cv <- read.table(paste(file,".CVlog",sep=""),as.is=T,sep = ":",header=F)
cv$K <- as.numeric(gsub(pattern = ")",replacement = "",x = gsub(pattern = "CV error (K=",replacement = "",x = cv$V1,fixed = T),fixed = T))
cv<-cv[order(as.numeric(cv$K)),]
plot(x=cv$K,y=cv$V2,type = "o", main="5-fold cross validated error",xlab="K",ylab="Prediction error",pch=19)
abline(v=8, col="red", lty=2, lwd= 1.5)
```
Now we plot the FST values for each k
```R
s <- scan(file = paste(file,".Keq",k,".CV",sep = ""),what = "complex",sep = "\n",quiet = T)
fst <- matrix(data = 0.0,nrow = k,ncol = k)
start <- which(s=="Fst divergences between estimated populations: ")+2
for (line in 1:k) {
  c <- strsplit(s[(start+line)],"\t")[[1]]
  fst[line+1,1:(length(c)-1)] <- as.numeric(c[2:length(c)])
}
fst[,k] <- as.numeric(rep(0.0,k))
par(mar=c(.1,.1,.1,.1))
plot(1:(k+3),1:(k+3),col=col[1:k],pch=15,cex=.0001,frame.plot=F,main="Modelled FST K=8",line=-2.3,xaxt="n",yaxt="n",xpd=T,ylab="",xlab="")
points(3:(k+1),rep(2,k-1),col=col[1:k-1],pch=15,cex=5,xpd=T)
points(rep(2,k-1),(k+1):3,col=col[2:k],pch=15,cex=5,xpd=T)
for (r in 1:nrow(fst)) {
  for (c in 1:ncol(fst)) {
    if (fst[k+1-r,c]!=0) {text(c+2,r+2,fst[k+1-r,c],font = 2)}
  }
}
```
dev.off outputs the second part of the figure
```R
dev.off()
```
The final result should look like this:
![Picture7](https://user-images.githubusercontent.com/89838666/201969546-d21db15a-2b9a-437d-af41-137059b5a925.jpg)

## Ancestry mapping 
Now we can use our admixture data to gnerate a geogrphic ancestry map

#Preparing the data
Firstly install and load data.table
```R
install.packages("data.table")
library(data.table)
```
Now run the Ancestry Density Function 
```R
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
```

Now set the working directory
```R
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
```
This function creates an input file for density maps using the admixture data
You need to specify the input tbl file, which comes from admixture results, Just write the prefix of the file (prefix.k.Q)
You also need to specify the best k 
The output is a table where the number of observations is replicated for each k
```R
Bindtbl <- ancestrydensity(file = "MasterLandraceTeoInbredGBS_collapseDist0.02_collapseSEED_Americas-TeoSubset_minTaxa01_admixfilt_rmvCloseKin0.03_poly_minSiteCov0.5_minTaxaCov0.3_RmvHighLD_rmvThird"
,order = fread("TaxaOrder_AL.txt", header=T),k = 8, latColumn="Latitude", lonColumn="Longitude")
```
Bindtbl contains the data from which we can generate an ancestry map

#Ancestry map plotting 
Install and load required packages
```R
install.packages("ggplot2")
install.packages("ggmap")
install.packages("maps")
install.packages("mapdata")
install.packages("readr")
install.packages("dplyr")
install.packages("data.table")
install.packages("colourvalues")
install.packages("svglite")

library(colourvalues)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(readr)
library(dplyr)
library(data.table)
library(svglite)
```
First we generate the georgaphic map, specifiying the coordinates and the map type
```R              
us_bbox2 <- c(left = -120, bottom = -40, right = -30, top = 40)
us_main_map <- get_stamenmap(us_bbox2, zoom = 5, maptype = "toner-lite")
```
Now out output the map
```R
out="KMapAL.svg"
svg(out)
```
You can alter bins to give you different levels of density on the map
Altering size changes the size of the point
Use scale fill to spceificy the colours for each k (match it to the colours used in the previous admixture plot)
```R
ggmap(us_main_map) +
  stat_density2d(aes(fill = label, alpha=..level..),
                                   geom = "polygon", bins=30,size =0.5, data = Bindtbl, contour=TRUE) +
  scale_fill_manual(values=c("K1"= "#59398d",
                             "K2" = "#6d83da",
                             "K3" = "#cf9d3a",
                             "K4" = "#b54673",
                             "K5" = "#45c097",
                             "K6" = "#ba4c46",
                             "K7" = "#968a3d",
                             "K8" = "#bb6130")) +
  geom_point(data = tbl, size=0.3, aes(x = lon, y = lat,)) +
  theme_classic()   
  ```
  dev.off outputs the map
  ```R
dev.off()
```
The final result should look like this:

![Picture8](https://user-images.githubusercontent.com/89838666/201974422-d827884d-ee6f-4cbf-a548-0ee7a3db9a53.jpg)
