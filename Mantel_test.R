install.packages("vegan")
library(vegan)
install.packages("geosphere")
library(geosphere)
geo = data.frame(andean$Long, andean$Lat)
d.geo = distm(geo, fun = distHaversine)
dist.geo = as.dist(d.geo)

#abundance vs geographic 
abund_geo  = mantel(dist.abund, dist.geo, method = "spearman", permutations = 9999, na.rm = TRUE)
abund_geo