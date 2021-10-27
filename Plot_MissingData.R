library(data.table)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
dataM <- fread("MasterLandraceTeoInbredGBS_collapseDist0.02_TaxaSummary.txt")
Missingdata<dataM$'Missing Gametes'
Missingdata
hist(Missingdata)
hist(Missingdata, main = "Missing Data")
hist(Missingdata, col = "lightblue")
hist(Missingdata, breaks=1, xlab="Missing data", 
     ylab="Frequency", main="Missing Data", col = "green")

