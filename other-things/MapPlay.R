install.packages(c("maps", "mapdata"))
devtools::install_github("dkahle/ggmap")
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(mapproj)
library(sf)
theme_set(theme_bw())
library("rnaturalearth")
library("rnaturalearthdata")
library(ggspatial)

world <- ne_countries(scale = "large", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(139.16, 139.36), ylim = c(35.42, 35.33), expand = FALSE)


#kanto and chubu area with tnt points
ggplot(data = world) +
  geom_sf(fill= "antiquewhite") +
  geom_point(data = tntpoints, aes(x = Long, y = Lat), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(137.15, 141.15), ylim = c(34.30, 36.30), expand = FALSE) +
  annotation_scale(location = "br", width_hint = 0.5) + 
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), 
                         style = north_arrow_fancy_orienteering)
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))
  
  #tnt area with tnt points
  ggplot(data = world) +
    geom_sf(fill= "antiquewhite") +
    geom_point(data = tntpoints, aes(x = Long, y = Lat), size = 4, 
               shape = 23, fill = "darkred") +
    coord_sf(xlim = c(139.16, 139.36), ylim = c(35.42, 35.33), expand = FALSE)
    annotation_scale(location = "br", width_hint = 0.5) + 
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), 
                           style = north_arrow_fancy_orienteering)
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))  

  
#trying ggmap with stamenmap

tntborder <- c(left = 139.16, right = 139.36, bottom = 35.33, top = 35.42)

map <- get_stamenmap(bbox = c(left = 139.14,  right = 139.40, 
                              top = 35.51, bottom = 35.30), zoom = 10, maptype = "toner-lite")
ggmap(map)


rnaturalearth::ne_c

japan <- map_data("japan")
ggplot() + geom_polygon(data = japan, aes(x=long, y = lat, group = group), fill = "lightgray", color = "black") + 
  coord_map()

tntpoints <- read_csv("data/raw-data/Tokyo PithouseDB V3.2 - Mapping.csv")
sbbox <- make_bbox(lon = tntpoints$Long, lat = tntpoints$Lat, f = .1)
tnt_map <- get_map(location = sbbox, maptype = "satellite", source = "osm")