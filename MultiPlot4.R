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
par(mfrow=c(2,1))

plot(x1,y1,
     xlab='Geographic Distance (km)', 
     ylab='Genetic Distance',
     main='Americas',
     ylim=c(0.10,0.16)
)

library("normalp")
abline(reg = lm(y1 ~ x1),col = "blue")

model <- lm(y1 ~ x1)

smod <- summary(model)
r21<-round(smod$r.squared, digits=6)
my.p1 <- cor.test(y1, x1)$p.value
smod
rp = vector('expression',2)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r21,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p1, digits = 2)))[2]

legend('topright', legend = rp, cex = 0.7, bty = 'n')
mtext("a",font = 2, side=side, line=line, cex=cex, adj=adj)

plot(x2,y2,
     xlab='Geographic Distance (km)', 
     ylab='Genetic Distance',
     main='South America',
     ylim=c(0.10,0.16)
)


abline(reg = lm(y2 ~ x2),col = "blue",)


model <- lm(y2 ~ x2)
smod <- summary(model)
r22<-round(smod$r.squared, digits=6)
my.p2 <- cor.test(y2, x2)$p.value
text("'0.5, 300'", paste("R2=",RR),cex = 1.5)

rp = vector('expression',2)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                   list(MYVALUE = format(r22,dig=3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(my.p2, digits = 2)))[2]

legend('topright', legend = rp, cex = 0.7, bty = 'n')
mtext("b",font = 2, side=side, line=line, cex=cex, adj=adj)


dev.off()

