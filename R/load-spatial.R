# Aim: load and explore spatial data

# Download data (commented as it's faster to download from browser)
# download.file("http://gsif.isric.org/zipped/SPCG2016_covs100m.rds",  "dat/SPCG2016_covs100m.rds") ## 310MB!
z = gzcon("dat/SPCG2016_covs100m.rds")
gzfile("dat/SPCG2016_covs100m.rds")
readRDS(z)
d = readRDS("dat/SPCG2016_covs100m.rds")
str(d)
r = raster(d)
