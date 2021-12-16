install.packages("wrapr")
library("wrapr")
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
out="GenvsGeoDist_Americas.svg"
svg(out)
DistFilt = mds_Americas$d
Gendistance <- DistFilt[,which(colnames(DistFilt)=="SEED_1463:D0KJYACXX:6:250070403")]
names(Gendistance) <- rownames(DistFilt)
#Gendistance <- DistFilt[-c(5962), ]
#DistFilt$`SEED_1463:D0KJYACXX:6:250070403`
Americaswithcentre2 <- Americaswithcentre[which(Americaswithcentre$Taxa %in% names(Gendistance)),]
#rm(list = ls())
x=Americaswithcentre2$distanceFromCentre[order(Americaswithcentre2$Taxa)]
y=DistFilt$`SEED_1463:D0KJYACXX:6:250070403`[order(names(Gendistance))]
plot(x,y,
     xlab='Geographic Distance (km)', 
     ylab='Genetic Distance',
     main='Americas'
     
)

library("normalp")
abline(reg = lm(y ~ x),col = "blue")

model <- lm(y ~ x)
smod <- summary(model)
RR<-round(smod$r.squared, digits=6)
pval <- cor.test(y, x)$p.value
text(0.5, 300, paste("R2=",RR),cex = 1.5)
dev.off()

DistFilt = DistFilt[-which(row.names(DistFilt) == "SEED_1463:D0KJYACXX:6:250070403"),]
Americaswithcentre = Americaswithcentre[-which(Americaswithcentre$TaxaNum == 14257 ),]
