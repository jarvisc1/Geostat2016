# Aim: create app data
source("R/load-spatial.R")
library(raster)
??resolution
plot(d)
points(p)
r = brick(d)
b = tmap::bb(d, ext = 0.2)
e = extent(b)
r = crop(x = r, e)
xmin = extent(r)[1]
ymin = extent(r)[3]
r = shift(r, x = -xmin, y = -ymin)
p = shift(p, x = -xmin, y = -ymin)
p = SpatialPointsDataFrame(coords = coordinates(p), data = p@data)
bbox(p)
bbox(r)
plotRGB(r)
points(p)

library(mapview)
m = mapview(r) 
m@map %>% leaflet::addCircleMarkers(data = p)

r1 = raster(r, layer = 1)
saveRDS(r, "geoapp1/raster-mini.Rds")
saveRDS(p, "geoapp1/training.Rds")
dput(
  as.vector(bb(r1, 0.8))
  )
initial_lat = 0.2081755
initial_lon = 25.331 
leaflet() %>% addCircles(data = p) %>% addRasterImage(r1) %>%
  setView(lng = initial_lon, lat = initial_lat, zoom = 11)
  # fitBounds(25.062339, 0.0377547280380721, 25.599651, 0.378596328038072)
