# Aim: load and explore spatial data

# Download covariate data (commented as it's faster to download from browser)
# download.file("http://gsif.isric.org/zipped/SPCG2016_covs100m.rds",  "dat/SPCG2016_covs100m.rds") ## 310MB!
d = readRDS("dat/SPCG2016_covs100m.rds")
str(d)
# plot(d) # commented out as takes time

# Load learning data
source("R/load-dat.R")
p = SpatialPointsDataFrame(cbind(learn$X, learn$Y), data = learn)
points(p)

