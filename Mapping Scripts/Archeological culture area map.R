library(sf)
library(ggplot2)
setwd("L:/Krisztian/QGIS")
aoi_boundary_HARV <- st_read("L:/Krisztian/QGIS/ArchaeologicalCultureAreas5.shp")

out="Cultmap.svg"
svg(out)
ggplot() + 
  geom_sf(data = aoi_boundary_HARV, size = 0.1, color = NA, fill = aoi_boundary_HARV$name1_shr) + 
  ggtitle("Archaeological Culture Areas") + 
  xlab("lat") + ylab("lon") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_sf(expand = FALSE)
dev.off()

