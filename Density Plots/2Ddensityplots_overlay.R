library(ggplot2)
library(grid)
library(gtable)

# plot with red fill
p1 <- ggplot(tblv3, aes(x = Longitude, y = Latitude)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", colour="white",size =0.01, data = tblv3) +
  scale_x_continuous(limits = c(-85, -55)) +
  scale_y_continuous(limits = c(-40, 18)) +
  scale_fill_gradient(low = "#c3678500", high = "#c36785",space = "Lab", name = "g = 1") +
  scale_colour_discrete(guide = "none") +
  theme_classic()
# plot with blue fill
p2 <- ggplot(tblv2, aes(x = Longitude, y = Latitude)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", colour="white",size =0.01, data = tblv2) +
  scale_x_continuous(limits = c(-85, -55)) +
  scale_y_continuous(limits = c(-40, 18)) +
  scale_fill_gradient(low = "#7f64b900", high = "#7f64b9",space = "Lab", name = "g = 2") +
  scale_colour_discrete(guide = "none") +
  theme_classic()
# grab plot data
pp1 <- ggplot_build(p1)
pp2 <- ggplot_build(p2)$data[[1]]
# replace red fill colours in pp1 with blue colours from pp2 when group is 2
pp1$data[[1]]$fill[grep(pattern = "^2", pp2$group)] <- pp2$fill[grep(pattern = "^2", pp2$group)]
# build plot grobs
grob1 <- ggplot_gtable(pp1)
grob2 <- ggplotGrob(p2)
# build legend grobs
leg1 <- gtable_filter(grob1, "guide-box") 
leg2 <- gtable_filter(grob2, "guide-box") 
leg <- gtable:::rbind_gtable(leg1[["grobs"]][[1]],  leg2[["grobs"]][[1]], "first")
# replace legend in 'red' plot
grob1$grobs[grob1$layout$name == "guide-box"][[1]] <- leg
# plot
grid.newpage()
grid.draw(grob1)


ggplot(tblv1, aes(x = Longitude, y = Latitude)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .3,
                 geom = "polygon", colour="white",size =0.01, data = tblv1) + 
  scale_x_continuous(limits = c(-85, -55)) +
  scale_y_continuous(limits = c(-40, 18)) +
  scale_fill_gradient(low = "#9f924400", high = "#9f9244") + 
  stat_density2d(aes(fill = ..level..), alpha = .3,
                 geom = "polygon", colour="white",size =0.01, data = tblv2) +
  scale_fill_gradient(low = "#7f64b900", high = "#7f64b9") +
  stat_density2d(aes(fill = ..level..), alpha = .3,
                 geom = "polygon", colour="white",size =0.01, data = tblv3) +
    scale_fill_gradient(low = "#c3678500", high = "#c36785")
