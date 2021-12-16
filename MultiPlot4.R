library(cowplot)
library(gridGraphics)
library("grid")
library("ggplotify")
source("L:/Krisztian/Scripts/TRGFunctionsv2.R")
out="multiplot4.svg"
svg(out)

line = 1
cex = 1
side = 3
adj=-0.2
par(mfrow=c(1,2))

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
mtext("a",font = 2, side=side, line=line, cex=cex, adj=adj)

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
mtext("b",font = 2, side=side, line=line, cex=cex, adj=adj)


dev.off()

