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
# points(p) # commented out as crashing
head(p)
saveRDS(p, "dat/p.Rds")

# load validation data
pv = SpatialPointsDataFrame(cbind(validate$X, validate$Y), data = validate)
head(pv)
saveRDS(pv, "dat/pv.Rds")



# Extract raster variables for training points
b <- brick(d)
trp1 <- raster::extract(b,  p)
rp <- as.data.frame(trp1)
rp$LNDCOV6_100m <- base::factor(as.factor(rp$LNDCOV6_100m), 
                                 levels = 1:28,
                                labels = levels(d$LNDCOV6_100m))

rp$PMTGSS7_100m <- base::factor(as.factor(rp$PMTGSS7_100m), 
                                 levels = 1:70,
                                 labels = levels(d$PMTGSS7_100m))

p_rp <- SpatialPointsDataFrame(p, data = rp)

saveRDS(p_rp, "dat/train_rp.RDS")

# Extract raster variables for validation points
vrp1 <- raster::extract(b,  pv)
vrp <- as.data.frame(vrp1)
vrp$LNDCOV6_100m <- base::factor(as.factor(vrp$LNDCOV6_100m), 
                                levels = 1:28,
                                labels = levels(d$LNDCOV6_100m))

vrp$PMTGSS7_100m <- base::factor(as.factor(vrp$PMTGSS7_100m), 
                                levels = 1:70,
                                labels = levels(d$PMTGSS7_100m))

p_vrp <- SpatialPointsDataFrame(pv, data = vrp)

head(p_vrp)
saveRDS(p_vrp, "dat/validate_rp.RDS")

# Extract Dem values
dem     <- raster('dat/DEMNED6_100m.sdat')
cprof   <- raster('dat/DEMNED6_100m_cprof.sdat')
devmean <- raster('dat/DEMNED6_100m_devmean.sdat')
openn   <- raster('dat/DEMNED6_100m_openn.sdat')
openp   <- raster('dat/DEMNED6_100m_openp.sdat')
slope   <- raster('dat/DEMNED6_100m_slope.sdat')
twi     <- raster('dat/DEMNED6_100m_twi.sdat')
vbf     <- raster('dat/DEMNED6_100m_vbf.sdat')
vdepth  <- raster('dat/DEMNED6_100m_vdepth.sdat')

demb <- brick(cprof, devmean, openn, openp, slope, twi, vbf, vdepth)

# Extract for training
trp2 <- raster::extract(demb,  p)
rp1 <- as.data.frame(trp2)

rp_all <- cbind(rp, rp1)

# Extract for validation
vrp3 <- raster::extract(demb,  pv)
vrp2 <- as.data.frame(vrp3)

vrp_all <- cbind(vrp, vrp1)
