library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
dataM <- fread("MasterLandraceTeoInbredGBS_collapseDist0.02_TaxaSummary.txt", data.table=F)
dataN <-fread("MasterLandraceTeoInbredGBS_collapseDist0.02_forMDSv2Test.txt", data.table=F)

out="Coverage_vs_Heterozygosity.svg"
svg(out)
landrace = unlist(fread("AllLandrace_No_BLANKS9.txt"))

dataN <- dataN[which(dataN$Taxa %in% landrace),]
dataM <- dataM[which(dataM$`Taxa Name` %in% landrace),]

dataM$Col <- dataN[match(dataM$`Taxa Name`, dataN$Taxa),"Geographic_color"]
dataM$Col <- tolower(dataM$Col)

dataM$Geographic <- dataN[match(dataM$`Taxa Name`, dataN$Taxa), "Geographic"]

#dataM <- dataM[which(dataM$`Taxa Name`%in%landrace),]
dataM[which(dataM$Col==""),"Col"] <- "black"


#dataM_noSEED <- dataM[-which(dataM$Col=="blue"),]
#dataM_noSEED$Col <- dataN[match(dataM_noSEED$`Taxa Name`, dataN$Taxa),"Geographic_color"]
#dataM_noSEED$Col <- tolower(dataM_noSEED$Col)

dataM$Geographic <- dataN[match(dataM$`Taxa Name`, dataN$Taxa), "Geographic"]
landrace_indices = match(landrace, dataM$`Taxa Name`, -1)
Coverage <- 1-dataM$'Proportion Missing'
ProportionHeterozygous <- dataM$`Proportion Heterozygous`[landrace_indices]

#par(mar=c(5.6, 6, 4.6, 2.6))
dataM$propPres <- 1-dataM$`Proportion Missing`
x=dataM$propPres
y=dataM$'Proportion Heterozygous'
plot(x,y,
     pch=20,
     cex=0.2, 
     col="Black",
     xlab="Coverage", ylab="Heterozygousity",
     main="Landrace Coverage vs Heterozygousity"
)
abline(reg = lm(y ~ x),col = "blue",)
#
plot(1-dataM$`Proportion Missing`, dataM$'Proportion Heterozygous',cex.main = 2, cex.lab = 2, cex.axis = 2,
     pch=20,
     cex=0.2, 
     col=dataM$Geographic,
     xlab="Coverage", ylab="Heterozygousity",
     main="Landrace Coverage vs Heterozygousity"
)
legend("topleft", 
       legend = dataM[!duplicated(dataM$Geographic),"Geographic"],
       col=dataM[!duplicated(dataM$Geographic),"Col"],
       pch = 20, 
       bty = "n", 
       pt.cex = 2, 
       cex = .7
       
)
       
dev.off()
plot(1-dataM_noSEED$`Proportion Missing`, dataM_noSEED$'Proportion Heterozygous',cex.main = 2, cex.lab = 2, cex.axis = 2,
     pch=20, 
     cex=0.2, 
     col=dataM_noSEED$Col,
     xlab="Coverage", ylab="Heterozygousity",
     main="Landrace Coverage vs Heterozygousity"
)
legend("topleft", 
       legend = dataM_noSEED[!duplicated(dataM_noSEED$Geographic),"Geographic"],
       col=dataM_noSEED[!duplicated(dataM_noSEED$Geographic),"Col"],
       pch = 20, 
       bty = "n", 
       pt.cex = 2, 
       cex = .7
       
)
