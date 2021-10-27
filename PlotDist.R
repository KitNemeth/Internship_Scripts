#install.packages("wrapr")
library("wrapr")
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
out="GenvsGeoDist_SA.svg"
svg(out)
Gendistance <- DistFilt[,which(colnames(DistFilt)=="SEED_1463:D0KJYACXX:6:250070403")]
names(Gendistance) <- rownames(DistFilt)
#Gendistance <- DistFilt[-c(5962), ]
#DistFilt$`SEED_1463:D0KJYACXX:6:250070403`
Andeanwithcentre2 <- andeanwithcentre[which(andeanwithcentre$Taxa %in% names(Gendistance)),]
#rm(list = ls())
x=Andeanwithcentre2$distanceFromCentre[order(Andeanwithcentre2$Taxa)]
y=DistFilt$`SEED_1463:D0KJYACXX:6:250070403`[order(names(Gendistance))]
plot(x,y,
     xlab='Geographic Distance (km)', 
     ylab='Genetic Distance',
     main='Genetic distance by geographic distance from Balsas'
     
)

abline(reg = lm(y ~ x),col = "blue")
dev.off()

DistFilt = DistFilt[-which(row.names(DistFilt) == "SEED_1463:D0KJYACXX:6:250070403"),]
andeanwithcentre = andeanwithcentre[-which(andeanwithcentre$Taxa == 14257 ),]
