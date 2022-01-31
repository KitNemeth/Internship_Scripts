library(dplyr)
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")

dataM <- as.data.frame(fread("MasterLandraceTeoInbredGBS_collapseDist0.02_forMDSv2Test.txt"))
dataM <- dataM[which(dataM$`Proportion Missing`<.95),]
dataM$Lat = as.numeric(dataM$Lat)
dataM$Long = as.numeric(dataM$Long)
dataM$Elev = as.numeric(dataM$Elev)

Americas <- dataM[which(dataM$Population%in%c("Landrace - Ames Stock Center","Landrace - RIMMA","Landrace - NSS","Landrace - AndeanMiguel","Landrace - SEED") & dataM$Country!=""  & dataM$Country!="0"),]
unique(Americas$Country)
new_America <- Americas %>% group_by(Country) %>% slice_sample(n=30)
Taxalist <- (new_America$Taxa, col.names = TRUE)
write.table(Taxalist, file = "TaxalistAmericas.txt", sep = "\t",
            row.names = FALSE)
