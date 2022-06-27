#### Rfunction launcher example for subsetting a data frame based on a geographic grid size to randomly sample over an area
### Requires a dataframe (key file), number of samples per grid, grid size and lattitude and longitude columns specified 
### Returns a subset of dataframe that has been sampled randomly across specified grid sizes with the number of idnividuals per grid specified 
source("L:/Krisztian/R Scripts/Functions/Subsetgrid Function.R")

setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")
dataM <- as.data.frame(fread("MasterLandraceTeoInbredGBS_collapseDist0.02_forMDSv3.txt"))

keep <- subsetGrid(dataM,1,0.5,"Lat", "Long")

Brazil <- dataM[which(dataM$Country%in%c("BRAZIL","Brazil") & dataM$Lat!=""  & dataM$Long!=""),] 
keep <- subsetGrid(Brazil,1,0.5,"lat", "lon")
