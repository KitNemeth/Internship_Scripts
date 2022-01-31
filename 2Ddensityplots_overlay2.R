library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(readr)
library(dplyr)
devtools::install_github("dkahle/ggmap")

tblv1$label <- "K1"
tblv2$label <- "K2"
tblv3$label <- "K3"
Bindtbl <- rbind(tblv2,tblv3,tblv1)
ggplot(Bindtbl, aes(x = lon, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = label), alpha = 0.4,
                 geom = "polygon", bins=50,size =0.01, data = Bindtbl) +
  scale_x_continuous(limits = c(-85, -55)) +
  scale_y_continuous(limits = c(-40, 18)) +
  geom_point() +
  theme_classic()                 

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap")
register_google(key = "AIzaSyBhNJuO6gnsT4M9s5Cm2upMy4sI5rrjVHA", write = TRUE)

myLocation = 'California'
myMap = get_map(location = myLocation, zoom = 6)
ggmap(myMap)

library(ggmap)

us_bbox <- c(left = -85, bottom = -40, right = -55, top = 18)
us_main_map <- get_stamenmap(us_bbox, zoom = 5, maptype = "terrain")
p_main <- ggmap(us_main_map)
p_main

ggmap(us_main_map) +
  stat_density2d(aes(fill = label), alpha = 0.4,
                                   geom = "polygon", bins=50,size =0.01, data = Bindtbl) +
  geom_point() +
  theme_classic() 

colnames(Bindtbl)[8] <- "lon"
colnames(Bindtbl)[9] <- "lat"

