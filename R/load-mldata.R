# Aim: load and explore spatial data

library(raster)
# Download covariate data (commented as it's faster to download from browser)
# download.file("http://gsif.isric.org/zipped/SPCG2016_covs100m.rds",  "dat/SPCG2016_covs100m.rds") ## 310MB!
d = readRDS("dat/SPCG2016_covs100m.rds")
str(d)
# plot(d) # commented out as takes time
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

# Make file raster bricks
b <- brick(d)
demb <- brick(cprof, devmean, openn, openp, slope, twi, vbf, vdepth)

# Clear Memory
rm(list =setdiff(ls(),c("b", "demb", "d")))

# Load Point data

# Load training data
source("R/load-dat.R")

# Training data
p = SpatialPointsDataFrame(cbind(learn$X, learn$Y), data = learn)
head(p)

# Validation data
pv = SpatialPointsDataFrame(cbind(validate$X, validate$Y), data = validate)
head(pv)

# Extract raster variables for training points
tbp <- raster::extract(b,  p)
tbp_df <- as.data.frame(tbp)
tdembp <- raster::extract(demb,  p)
tdembp_df <- as.data.frame(tdembp)
tbp_df <- cbind(tbp_df, tdembp_df)

tbp_df$LNDCOV6_100m <- base::factor(as.factor(tbp_df$LNDCOV6_100m), 
                                levels = 1:28,
                                labels = levels(d$LNDCOV6_100m))

tbp_df$PMTGSS7_100m <- base::factor(as.factor(tbp_df$PMTGSS7_100m), 
                                levels = 1:70,
                                labels = levels(d$PMTGSS7_100m))

tbp_p <- SpatialPointsDataFrame(p, data = tbp_df)


# Extract raster variables for validation points
vbp <- raster::extract(b,  pv)
vbp_df <- as.data.frame(vbp)
tdembp <- raster::extract(demb,  pv)
tdembp_df <- as.data.frame(tdembp)
vbp_df <- cbind(vbp_df, tdembp_df)

vbp_df$LNDCOV6_100m <- base::factor(as.factor(vbp_df$LNDCOV6_100m), 
                                levels = 1:28,
                                labels = levels(d$LNDCOV6_100m))

vbp_df$PMTGSS7_100m <- base::factor(as.factor(vbp_df$PMTGSS7_100m), 
                                levels = 1:70,
                                labels = levels(d$PMTGSS7_100m))

vbp_p <- SpatialPointsDataFrame(pv, data = vbp_df)




# PCA on numerica variables
tbp_dfPCA <- tbp_df
tbp_dfPCA$LNDCOV6_100m <- NULL
tbp_dfPCA$PMTGSS7_100m <- NULL

tbp_pca <- princomp(tbp_dfPCA)
tbp_pca <- as.data.frame(tbp_pca$scores) 

vbp_dfPCA <- vbp_df
vbp_dfPCA$LNDCOV6_100m <- NULL
vbp_dfPCA$PMTGSS7_100m <- NULL

vbp_pca <- princomp(vbp_dfPCA)
vbp_pca <- as.data.frame(vbp_pca$scores) 

learn$TAXOUSDA <- factor(learn$TAXOUSDA)
learn$TAXNUSDA <- factor(learn$TAXNUSDA)

# Clear Memory
rm(list =setdiff(ls(),c("learn", "validate", "tbp_p", "vbp_p", "tbp_df", "vbp_df",
                        "tbp_pca", "vbp_pca", "p", "pv")))


save.image(file = "dat/ML.rdata")



