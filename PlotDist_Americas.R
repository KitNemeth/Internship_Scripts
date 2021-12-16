install.packages("wrapr")
library("wrapr")
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
out="GenvsGeoDist_Americas.svg"
svg(out)
DistFilt = mds_Americas$d
DistFilt = DistFilt[-which(row.names(DistFilt) == "SEED_1463:D0KJYACXX:6:250070403"),]
Gendistance <- DistFilt[,which(colnames(DistFilt)=="SEED_1463:D0KJYACXX:6:250070403")]
names(Gendistance) <- rownames(DistFilt)
#Gendistance <- DistFilt[-c(5962), ]
#DistFilt$`SEED_1463:D0KJYACXX:6:250070403`
Americaswithcentre = Americaswithcentre[-which(Americaswithcentre$TaxaNum == 14257 ),]
Americaswithcentre2 <- Americaswithcentre[which(Americaswithcentre$Taxa %in% names(Gendistance)),]
#rm(list = ls())
x1=Americaswithcentre2$distanceFromCentre[order(Americaswithcentre2$Taxa)]
y1=DistFilt$`SEED_1463:D0KJYACXX:6:250070403`[order(names(Gendistance))]
plot(x1,y1,
     xlab='Geographic Distance (km)', 
     ylab='Genetic Distance',
     main='Americas',
     ylim=c(0.10,0.16)
)

library("normalp")
abline(reg = lm(y ~ x),col = "blue")

model <- lm(y1 ~ x1)

smod <- summary(model)
r2<-round(smod$r.squared, digits=6)
my.p <- cor.test(y1, x1)$p.value
smod
rp = vector('expression',2)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]

legend('topright', legend = rp, cex = 0.7, bty = 'n')

text(0.5, 300, paste("R2=",RR),cex = 1.5)
dev.off()

