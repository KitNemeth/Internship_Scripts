install.packages("geosphere")
library(geosphere)
#andean$BalsaLat <- rep("17.937329584",nrow(andean))
#andean$BalsaLong <- rep("-102.135999456",nrow(andean))

#andean$DistanceBalsas <- as.vector(distm(c(andean$BalsaLong, andean$BalsaLat), c(andean$Long, andean$Lat),fun = distHaversine))

#distGeo(andean[,13:12], andean[,25:24])
                                
install.packages("dplyr")
library(dplyr)
options("scipen"=100, "digits"=4)
library(geosphere)
centre = c(-97.62, 16.12)
aTaxa = Americas %>% slice(1)
aTaxa
distHaversine(c(aTaxa$Long, aTaxa$Lat), centre) 

distanceFromCentre = by(Americas, 1:nrow(Americas), function(row) { distHaversine(c(row$Long, row$Lat), centre)  })

distanceFromCentre %>% head()

Americaswithcentre = Americas %>% 
  mutate(distanceFromCentre = by(Americas, 1:nrow(Americas), function(row) { distHaversine(c(row$Long, row$Lat), centre)  }))

Americaswithcentre %>% head()
Americaswithcentre$distanceFromCentre <- as.numeric(Americaswithcentre$distanceFromCentre)
