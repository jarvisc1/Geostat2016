# Aim: load and explore spatial data

library(raster)
# Download covariate data (commented as it's faster to download from browser)
# download.file("http://gsif.isric.org/zipped/SPCG2016_covs100m.rds",  "dat/SPCG2016_covs100m.rds") ## 310MB!
d = readRDS("dat/SPCG2016_covs100m.rds")
str(d)
# plot(d) # commented out as takes time

# Load learning data
source("R/load-dat.R")
p = SpatialPointsDataFrame(cbind(learn$X, learn$Y), data = learn)
points(p)
head(p)
saveRDS(p, "dat/p.Rds")


# Turn d into a raster brick for manipulation
b <- brick(d)

# Get coordinate data for training points
xy <- data.frame(x =p$X, y = p$Y) 

# Extract raster variables for testing points
# test raster points
trp1 <- raster::extract(b,  p)

# Format as dataframe and save as spatial points
rp <- as.data.frame(trp1)
rp$LNDCOV6_100m <- base::factor(as.factor(rp$LNDCOV6_100m), 
                                 levels = 1:28,
                                labels = levels(d$LNDCOV6_100m))

rp$PMTGSS7_100m <- base::factor(as.factor(rp$PMTGSS7_100m), 
                                 levels = 1:70,
                                 labels = levels(d$PMTGSS7_100m))

## save a spatial points with raster info for   
p_rp <- SpatialPointsDataFrame(p, data = rp)

saveRDS(p_rp, "dat/train_rp.RDS")

# repeat process for testing points
