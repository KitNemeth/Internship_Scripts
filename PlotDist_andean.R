#install.packages("wrapr")
library("wrapr")
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
out="GenvsGeoDist_SA.svg"
svg(out)
DistFilt = mds_andean$d
DistFilt = DistFilt[-which(row.names(DistFilt) == "SEED_1463:D0KJYACXX:6:250070403"),]
Gendistance <- DistFilt[,which(colnames(DistFilt)=="SEED_1463:D0KJYACXX:6:250070403")]
names(Gendistance) <- rownames(DistFilt)
#Gendistance <- DistFilt[-c(5962), ]
#DistFilt$`SEED_1463:D0KJYACXX:6:250070403`
andeanwithcentre = andeanwithcentre[-which(andeanwithcentre$TaxaNum == 14257 ),]
Andeanwithcentre2 <- andeanwithcentre[which(andeanwithcentre$Taxa %in% names(Gendistance)),]
#rm(list = ls())
x2=Andeanwithcentre2$distanceFromCentre[order(Andeanwithcentre2$Taxa)]
y2=DistFilt$`SEED_1463:D0KJYACXX:6:250070403`[order(names(Gendistance))]
plot(x2,y2,
     xlab='Geographic Distance (km)', 
     ylab='Genetic Distance',
     main='South America',
     ylim=c(0.107,0.145)
)


abline(reg = lm(y2 ~ x2),col = "blue",)


model <- lm(y2 ~ x2)
smod <- summary(model)
r2<-round(smod$r.squared, digits=6)
my.p <- cor.test(y2, x2)$p.value
text("'0.5, 300'", paste("R2=",RR),cex = 1.5)

rp = vector('expression',2)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r2,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p, digits = 2)))[2]

legend('topright', legend = rp, cex = 0.7, bty = 'n')

legend("topright",
       legend = c("Highland", "Lowland"), 
       col = c('darkorchid1', 
               'darkorchid4'), 
       pch = c(19) 
       )

dev.off()

# col= ifelse(Andeanwithcentre2$Elev < 1700,'darkorchid4','darkorchid')
