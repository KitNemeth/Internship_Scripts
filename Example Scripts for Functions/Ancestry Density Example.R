source("L:/Krisztian/R Scripts/Functions/Ancestry Density Function.R")
setwd("L:/Krisztian/MasterLandraceTeoInbredGBS_collapseDist0.02_20210810")

#objective of the function, creates input file for density maps

#input tbl file comes from admixture results, Just write the prefix of the file (prefix.k.Q)
#you should put the best K 

#output is a table where the number of observations is replicated for each K

Bindtbl <- ancestrydensity(file = "MasterLandraceTeoInbredGBS_collapseDist0.02_collapseSEED_ALwithteo_subset_minTaxa01_admixfilt_rmvCloseKin0.03_poly_minSiteCov0.5_minTaxaCov0.3_RmvHighLD_rmvThird"
,order = fread("TaxaOrder_AL.txt", header=T),k = 15, latColumn="Latitude", lonColumn="Longitude")
