#install.packages("wrapr")
library("wrapr")
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
out="GenvsGeoDist_SA.svg"
svg(out)
DistFilt = mds_andean$d
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
     main='Genetic distance by geographic distance from Balsas',
     ylim=c(0.107,0.145)
)


abline(reg = lm(y ~ x),col = "blue",)


model <- lm(y ~ x)
smod <- summary(model)
RR<-round(smod$r.squared, digits=6)
pval <- cor.test(y, x)$p.value
text(0.5, 300, paste("R2=",RR),cex = 1.5)

legend("topright",
       legend = c("Highland", "Lowland"), 
       col = c('darkorchid1', 
               'darkorchid4'), 
       pch = c(19) 
       )

dev.off()

DistFilt = DistFilt[-which(row.names(DistFilt) == "SEED_1463:D0KJYACXX:6:250070403"),]
andeanwithcentre = andeanwithcentre[-which(andeanwithcentre$TaxaNum == 14257 ),]
# col= ifelse(Andeanwithcentre2$Elev < 1700,'darkorchid4','darkorchid')
