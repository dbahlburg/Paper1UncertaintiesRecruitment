# This script contains a template for a stereographic pseudo 3D-projection of 
# the Southern Ocean. This map could be used for illustrating the geographical
# distribution of the krill literature. It is not straightforward to assign the
# respective papers into geographical regions since the marginal seas of the 
# Southern Ocean are not clearly defined and accepted.
# As an alternative, it could be useful to map the studies onto the FAO-areas
# which are used for active resource management in the Southern Ocean. However,
# their disadvantage are their cryptic names and there differing spatial extends
# which leads to visual over- and underrepresentation of certain regions.

# Load dependencies
library(tidyverse)
library(geosphere)
library(rgdal)
library(here)

# load world map and fao-areas
world <- map_data("world")
faoAreas <- fortify(readOGR(here('infographicsGroup','inputData','FAO_Areas_ShapefilesWGS84','asd-shapefile-WGS84.shp')))

# assign dummy value to fao-area (this could be the number of studies)
numberOfStudiesFAO <- faoAreas %>% 
  distinct(group) %>% 
  mutate(numberOfStudies = round(runif(n = 19, min = 1, max = 12)))
faoAreas <- faoAreas %>% 
  left_join(., numberOfStudiesFAO, by = 'group')

# Given the long/lat coordinates of an origin (x) and a radius (radius) in km,
# returns the coordinates of 360 points on the circle of center x and radius radius km.
# --
# x (numeric vector) : coordinates of the origin of the circle
# radius (numeric) : radius of the circle
# --
distantCircle <- function(x, radius) {
  # Creation de 360 points distincts sur le cercle de centre
  # x et de rayon radius
  resul <- do.call("rbind", lapply(0:360, function(bearing) {
    res <- destPoint(p = x, b = bearing, d = radius)
    rownames(res) <- NULL
    return(data.frame(res))
  }))
  resul$dist <- radius / 1000
  return(resul)
}

#get circles at specified latitudes (inside cos)
circleLats <- 6378137 * cos(c(-80,-50,-20) * pi/180)

circle.6000 <- distantCircle(x = c(0.0000001,89.9999999), radius = 4450*1000)
circle.9000 <- distantCircle(x = c(0.0000001,89.9999999), radius = 7780*1000)
circle.12000 <- distantCircle(x = c(0.0000001,89.9999999), radius = 11110*1000)
circles <- rbind(circle.6000, circle.9000, circle.12000) %>% 
  mutate(lat = -lat)

#define xLines for longitudinal grid
xLines <- seq(-180,120, by=60)

#define "dummy grid" for background color of map (ocean colour)
oceanColour <- expand_grid(lat = seq(-86,60, length.out = 20),
                           long = seq(-180,180, length.out = 20)) %>% 
  mutate(value = 1)

#execute plot function, store to object
faoAreasPlot <- ggplot() +
  geom_tile(data = oceanColour, aes(x = long, y = lat), fill = '#fcfcfc',
            colour = '#fcfcfc')+
  geom_polygon(data = faoAreas, aes(x = long, y = lat, 
                                    group = group, 
                                    fill = numberOfStudies,
                                    colour = numberOfStudies),
               size = 0.15) +
  scale_fill_gradient(low = '#c9c9c9',high = '#6ca85d') +
  geom_segment(aes(y = rep(30, times = length(xLines)), yend = rep(-90, times = length(xLines)), 
                   x = xLines, xend = xLines), 
               size = 0.5,
               colour = '#808080',
               alpha = 0.2,
               linetype = '82') +
  geom_path(data = circles, aes(x = lon, y = lat, group = dist),
            size = 0.5, colour = '#808080', linetype = '82', alpha = 0.2)+
  geom_polygon(data = world, aes(long, lat,group=group), 
               fill="#949494",color= '#bababa', size = 0.01) +
  coord_map("ortho",orientation = c(-110, 10, 0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'transparent', colour = NA),
        plot.background = element_rect(fill = 'transparent', colour = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'none')

#save plot
ggsave(here('infographicsGroup','graphics','SouthernOceanStereoTemplate.png'), plot = faoAreasPlot, bg = 'transparent', width = 6, height = 6)




