# Aim: create app data
source("R/load-spatial.R")
library(raster)
plot(d)
points(p)
points(p_vrp, col = "grey")
r = brick(d)
b = tmap::bb(d, ext = 0.2)
e = extent(b)
r = crop(x = r, e)
xmin = extent(r)[1]
ymin = extent(r)[3]
r = shift(r, x = -xmin, y = -ymin)
p = shift(p, x = -xmin, y = -ymin)
v = shift(p_vrp, x = -xmin, y = -ymin)
p = SpatialPointsDataFrame(coords = coordinates(p), data = p@data)
v = SpatialPointsDataFrame(coords = coordinates(v), data = v@data)
bbox(p)
bbox(r)
plotRGB(r)
points(p)
bpol = stplanr::bb2poly(r)
proj4string(bpol) = proj4string(r)
p = p[bpol,]
v = v[bpol,]
plot(p)
plot(v)

# saveRDS(r, "geoapp1/raster-mini.Rds")
# saveRDS(p, "geoapp1/training.Rds")
# saveRDS(v, "geoapp1/v.Rds")

dput(
  as.vector(bb(r1, 0.8))
  )
initial_lat = 0.2081755
initial_lon = 25.331 
leaflet() %>% addCircles(data = p) %>% addRasterImage(r1) %>%
  setView(lng = initial_lon, lat = initial_lat, zoom = 11)
  # fitBounds(25.062339, 0.0377547280380721, 25.599651, 0.378596328038072)


